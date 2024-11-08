
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

(define (infer-csv-grammar file)
  (call-with-input-file file
    (lambda (in)
      default-csv-grammar)))

(define (infer-csv-types file grammar num-cols numeric-storage)
  (call-with-input-file file
    (lambda (in)
      (make-vector num-cols generic-storage-class))))

(define (csv-read->table-row! parser table row-num)
  (lambda (in)
    (parser (lambda (acc col-num field)
              (table-set! table row-num col-num field)
              #t)
            #t
            in)))

(define (table-load-csv file . o)
  (let-keywords* o ((grammar (infer-csv-grammar file))
                    (parser (csv-parser grammar))
                    (header-from-first-line? #f)
                    (default-storage #f)
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
                    (types (if default-storage
                               (make-vector num-cols default-storage)
                               (infer-csv-types file
                                                grammar
                                                num-cols
                                                numeric-storage))))
    (let*-values (((arrays type-specs) (types->arrays types num-rows num-cols))
                  ((table) (make-table arrays type-specs labels '#())))
      (call-with-input-file file
        (lambda (in)
          (when header-from-first-line?
            (csv-skip-line in grammar))
          (let lp ((row-num 0))
            (unless (eof-object?
                     ((csv-read->table-row! parser table row-num) in))
              (lp (+ row-num 1))))))
      table)))
