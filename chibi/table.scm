
(define-record-type Table
  (make-table arrays type-specs labels indexes)
  table?
  ;; vector of 1 or more 2d homogeneous arrays with the same number of rows
  (arrays table-arrays table-arrays-set!)
  ;; vector of length n, the number of columns in the arrays,
  ;; where each element is a pair of the index in the above arrays table,
  ;; and the column index within that array
  (type-specs table-type-specs table-type-specs-set!)
  ;; vector of length n, the number of columns in the arrrays,
  ;; holding symbolic names that can be used in place of the
  ;; column indexes
  (labels table-labels table-labels-set!)
  ;; TODO: limit the indexes to an integer set
  (indexes table-indexes table-indexes-set!))

(define (table-num-rows table)
  (interval-width (array-domain (vector-ref (table-arrays table) 0)) 0))

(define (table-num-columns table)
  (vector-length (table-type-specs table)))

(define (table-shape table)
  (list (table-num-rows table) (table-num-columns table)))

;; Given an ordered vector of storage classes, groups them mapping
;; their corresponding indices and returns two values: a vector of
;; homogeneous arrays for each storage class and a vector of type
;; specs.
(define types->arrays
  (opt*-lambda (types
                num-rows
                (num-cols (vector-length types))
                (init-array (lambda (a indices) a))
                (get-storage-class (lambda (i) generic-storage-class)))
    (let lp ((i 0)
             (storages '())
             (specs '()))
      (cond
       ((= i num-cols)
        (let ((arrays
               (map (lambda (x)
                      (init-array
                       (make-specialized-array
                        (make-interval (vector num-rows (length (cdr x))))
                        (car x))
                       (reverse (cdr x))))
                    (reverse storages))))
          (values (list->vector arrays)
                  (list->vector (reverse specs)))))
       (else
        (let ((storage-class (if (< i (vector-length types))
                                 (vector-ref types i)
                                 (get-storage-class i))))
          (cond
           ((find-tail (lambda (cell) (eq? (car cell) storage-class)) storages)
            => (lambda (tail)
                 (let ((cell (car tail)))
                   (set-cdr! cell (cons i (cdr cell)))
                   (lp (+ i 1)
                       storages
                       (cons (cons (length (cdr tail)) (length (cddr cell)))
                             specs)))))
           (else
            (lp (+ i 1)
                (cons (list storage-class i) storages)
                (cons (cons (length storages) 0) specs))))))))))

;; vectors should be a vector of vectors of the same length
;; types is a vector of predetermined column types
;; - an error is signalled if one of the column types doesn't
;;   match the data
;; - if types is shorter than the number of columns, the
;;   remaining types are generic
;; returns a vector of arrays, and corresponding vector of
;;   type specs
(define (vector*->typed-arrays vectors types)
  (let ((num-rows (vector-length vectors))
        (num-cols (vector-length (vector-ref vectors 0))))
    (types->arrays types
                   num-rows
                   num-cols
                   (lambda (a indices)
                     (let ((set-a (array-setter a)))
                       (let lp ((ls indices)
                                (i 0))
                         (cond
                          ((pair? ls)
                           (do ((j 0 (+ j 1)))
                               ((= j num-rows))
                             (set-a (vector-ref (vector-ref vectors j)
                                                (car ls))
                                    j
                                    i))
                           (lp (cdr ls) (+ i 1)))
                          (else a))))))))

;; types should be a vector of storage per column, default to
;; generic-storage-class
(define table
  (opt*-lambda (data labels (types '#()))
    (call-with-values
        (lambda ()
          (if (array? data)
              (values (vector data)
                      (vector-map
                       (lambda (i) (cons data i))
                       (list->vector
                        (iota (interval-width (array-domain data))
                              (interval-lower-bound (array-domain data) 1)))))
              (vector*->typed-arrays data types)))
      (lambda (arrays types)
        (make-table arrays
                    types
                    labels
                    '#())))))

(define (table-resolve-index table row col)
  (let* ((col-index
          (if (integer? col)
              col
              (vector-index (lambda (x) (eq? x col))
                            (table-labels table))))
         (spec (vector-ref (table-type-specs table) col-index)))
    (values (vector-ref (table-arrays table) (car spec))
            row
            (cdr spec))))

(define (table-ref table row col)
  (call-with-values (lambda () (table-resolve-index table row col))
    (lambda (array row col)
      (array-ref array row col))))

(define (table-set! table row col value)
  (call-with-values (lambda () (table-resolve-index table row col))
    (lambda (array row col)
      (array-set! array value row col))))

(define (table-column table column)
  (call-with-values (lambda () (table-resolve-index table 0 column))
    (lambda (array row col)
      (array-extract
       array
       (make-interval (vector 0 col)
                      (vector (table-num-rows table) (+ col 1)))))))

(define (table-column-storage table column)
  (call-with-values (lambda () (table-resolve-index table 0 column))
    (lambda (array row col) (array-storage-class array))))

;;> Returns a single homogenous array containing the requested columns
;;> of the table, in order, using the most specific storage class
;;> possible.
(define table->array
  (opt*-lambda (table (labels (table-labels table)))
    (if (and (= 1 (vector-length (table-arrays table)))
             (equal? labels (table-labels table)))
        (vector-ref (table-arrays table) 0)
        ;; TODO: If the columns are contiguous and of the same storage
        ;; class we can just array-extract them.
        (let* ((columns (vector-map (lambda (column) (table-column table column))
                                    labels))
               (storage (widest-storage-class (vector->list columns)))
               (domain (make-interval (vector 0 0)
                                      (vector (table-num-rows table)
                                              (vector-length columns))))
               (array (make-specialized-array domain storage)))
          (do ((i 0 (+ i 1)))
              ((= i (vector-length columns)) array)
            (array-assign!
             (array-to-origin
              (array-extract
               array
               (make-interval (vector 0 i)
                              (vector (table-num-rows table) (+ i 1)))))
             (array-to-origin
              (vector-ref columns i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define potential-separators (string->list ",;:!|\t/$"))

(define (vector-sum vec) (vector-fold + 0 vec))

(define (infer-csv-grammar-from-counts counts)
  ;; For each of the potential separators, consider it's average count
  ;; per record and take the MAE of the difference between each record
  ;; and the average.  Choose the separator with the smallest error,
  ;; breaking ties with the most common separator.
  (let lp ((ls counts)
           (min-err +inf.0)
           (min-sum +inf.0)
           (min-sep #\,))
    (if (null? ls)
        ;; TODO: Consider inferring quoting.
        (csv-grammar `((separator-chars ,min-sep)))
        (let* ((sep (caar ls))
               (count-per-line (cdar ls))
               (sum (vector-sum count-per-line)))
          (if (> sum 0)
              (let* ((avg (/ sum (vector-length count-per-line)))
                     (mae (vector-sum
                           (vector-map (lambda (x) (abs (- avg x)))
                                       count-per-line))))
                (if (< mae min-err)
                    (lp (cdr ls) mae sum sep)
                    (lp (cdr ls) min-err min-sum min-sep)))
              (lp (cdr ls) min-err min-sum min-sep))))))

;; currently assumes crlf or lf as the record separator
(define (infer-csv-grammar x . o)
  (let ((max-lines (if (pair? o) (car o) 5)))
    (define (infer in)
      (let ((counts (map (lambda (ch) (cons ch (make-vector max-lines 0)))
                         potential-separators)))
        (let lp ((line 0))
          (let ((ch (read-char in)))
            (cond
             ((eof-object? ch)
              (infer-csv-grammar-from-counts
               (if (< line 4)
                   (map (lambda (x) (cons (car x) (vector-copy (cdr x) 0 line)))
                        counts)
                   counts)))
             ((eqv? ch #\newline)
              (if (>= (+ line 1) max-lines)
                  (infer-csv-grammar-from-counts counts)
                  (lp (+ line 1))))
             ((assv ch counts)
              => (lambda (cell)
                   (vector-set! (cdr cell)
                                line
                                (+ 1 (vector-ref (cdr cell) line)))
                   (lp line)))
             (else
              (lp line)))))))
    (if (input-port? x)
        (infer x)
        (call-with-input-file x infer))))

(define (infer-storage? str numeric-storage)
  (cond
   ((string->number str) numeric-storage)
   ;; TODO: Consider supporting TRUE/FALSE booleans.
   (else generic-storage-class)))

(define (csv-read->storage parser num-cols numeric-storage)
  (lambda (in)
    (let ((res (make-vector num-cols generic-storage-class)))
      (let ((len (parser
                  (lambda (prev-i i field)
                    (let ((storage (infer-storage? field numeric-storage)))
                      (vector-set! res i storage)
                      i))
                  0
                  in)))
        (if (or (eof-object? len) (zero? len))
            (eof-object)
            res)))))

(define (infer-csv-types file parser num-cols numeric-storage . o)
  (let ((max-lines (if (pair? o) (car o) 50)))
    (call-with-input-file file
      (lambda (in)
        ;; just throw away the first line in case it's a header
        (let ((maybe-header ((csv-read->vector parser) in))
              (reader (csv-read->storage parser num-cols numeric-storage)))
          (let lp ((line 1)
                   (res-storages (reader in)))
            (if (>= line max-lines)
                res-storages
                (let ((storages (reader in)))
                  (if (eof-object? storages)
                      res-storages
                      (lp (+ line 1)
                          (vector-map storage-class-fit res-storages storages))
                    )))))))))

(define (csv-read->table-row! parser table converters row-num)
  (lambda (in)
    (parser
     (lambda (acc col-num field)
       (let ((val (guard (exn (else (if (equal? "" field) 0.0 +nan.0)))
                    ((vector-ref converters col-num) field))))
         (table-set! table row-num col-num val)
         #t))
     #t
     in)))

(define (table-load-csv file . o)
  (let-keywords* o ((grammar (infer-csv-grammar file))
                    (parser (csv-parser grammar))
                    (header-from-first-line? #f)
                    (storage #f)
                    (numeric-storage f32-storage-class)
                    (num-rows (- (call-with-input-file file
                                   (lambda (in) (csv-num-rows grammar in)))
                                 (if header-from-first-line? 1 0)))
                    (labels
                     (call-with-input-file file
                       (lambda (in)
                         (let ((row ((csv-read->vector parser) in)))
                           (if header-from-first-line?
                               (vector-map string->symbol row)
                               (vector-unfold
                                (lambda (i)
                                  (string-append "col-" (number->string i)))
                                (vector-length row)))))))
                    (num-cols (vector-length labels))
                    (types (if storage
                               (make-vector num-cols storage)
                               (infer-csv-types file
                                                parser
                                                num-cols
                                                numeric-storage)))
                    ;; TODO: If the numeric fields coincide with
                    ;; unquoted fields, use 'quote-non-numeric? #t.
                    (converters
                     (vector-map
                      (lambda (type)
                        (cond
                         ((eq? generic-storage-class type)
                          (lambda (field) field))
                         ((inexact? (storage-class-default type))
                          (lambda (field) (inexact (string->number field))))
                         (else string->number)))
                      types)))
    (let*-values (((arrays type-specs) (types->arrays types num-rows num-cols))
                  ((table) (make-table arrays type-specs labels '#())))
      (call-with-input-file file
        (lambda (in)
          (when header-from-first-line?
            (csv-skip-line in grammar))
          (let lp ((row-num 0))
            (unless (eof-object?
                     ((csv-read->table-row! parser table converters row-num) in))
              (lp (+ row-num 1))))))
      table)))
