
(define-record-type Table
  (make-table arrays types labels indexes)
  table?
  ;; vector of 1 or more 2d homogeneous arrays with the same number of rows
  (arrays table-arrays table-arrays-set!)
  ;; vector of length n, the number of columns in the arrays,
  ;; where each element is a pair of the index in the above arrays table,
  ;; and the column index within that array
  (types table-types table-types-set!)
  ;; vector of length n, the number of columns in the arrrays,
  ;; holding symbolic names that can be used in place of the
  ;; column indexes
  (labels table-labels table-labels-set!)
  ;; TODO: limit the indexes to an integer set
  (indexes table-indexes table-indexes-set!))

(define-syntax table-typed-axis
  (syntax-rules ()
    ((table-typed-axis table) 1)))

(define (table-num-rows table)
  (interval-width (array-domain (vector-ref (table-arrays table) 0)) 0))

(define (table-num-columns table)
  (vector-length (table-types table)))

(define (table-shape table)
  (list (table-num-rows table) (table-num-columns table)))

;; vectors should be a vector of vectors of the same length
;; types is a vector of predetermined column types
;; - an error is signalled if one of the column types doesn't
;;   match the data
;; - if types is shorter than the number of columns, the
;;   remaining types are inferred
;; returns a vector of arrays, and corresponding vector of
;;   type specs
(define (vector*->typed-arrays vectors types)
  (let ((num-rows (vector-length vectors))
        (num-cols (vector-length (vector-ref vectors 0))))
    (let lp ((i 0)
             (storages '())
             (specs '()))
      (cond
       ((= i num-cols)
        (let ((arrays
               (map (lambda (x)
                      (let* ((a
                              (make-specialized-array
                               (make-interval
                                (vector num-rows (length (cdr x))))
                               (car x)))
                             (set-a (array-setter a)))
                        ;; copy each vector column into the array
                        (let lp ((ls (reverse (cdr x)))
                                 (i 0))
                          (when (pair? ls)
                            (do ((j 0 (+ j 1)))
                                ((= j num-rows))
                              (set-a (vector-ref (vector-ref vectors j)
                                                 (car ls))
                                     j
                                     i))
                            (lp (cdr ls) (+ i 1))))
                        a))
                    (reverse storages))))
          (values (list->vector arrays)
                  (list->vector (reverse specs)))))
       (else
        (let ((storage-class (if (< i (vector-length types))
                                 (vector-ref types i)
                                 ;; TODO: infer narrowest type?
                                 generic-storage-class)))
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

(define (table-resolve-index table multi-index)
  (let lp ((i 0)
           (ls multi-index)
           (res-index '())
           (array #f))
    (cond
     ((null? ls)
      (values array (reverse res-index)))
     (else
      (let ((index (if (integer? (car ls))
                       (car ls)
                       (vector-index (lambda (x) (eq? x (car ls)))
                                     (table-labels table)))))
        (if (= i (table-typed-axis table))
            (let ((spec (vector-ref (table-types table) index)))
              (lp (+ i 1)
                  (cdr ls)
                  (cons (cdr spec) res-index)
                  (vector-ref (table-arrays table) (car spec))))
            (lp (+ i 1) (cdr ls) (cons index res-index) array)))))))

(define (table-ref table . multi-index)
  (call-with-values (lambda () (table-resolve-index table multi-index))
    (lambda (array multi-index)
      (apply array-ref array multi-index))))

(define (table-set! table value . multi-index)
  (call-with-values (lambda () (table-resolve-index table multi-index))
    (lambda (array multi-index)
      (apply array-set! array value multi-index))))
