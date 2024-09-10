
;;> Linear algebra and extended array routines.

(define-library (chibi math linalg)
  (import (scheme base) (scheme inexact) (scheme list) (scheme write)
          (srfi 33) (srfi 231)
          (chibi assert) (chibi optional))
  (export array= array-append array-stack identity-array
          array-to-origin array-transpose array-first
          array-inverse determinant
          array-broadcast array-squeeze
          array-mul array-mul! array-mul-expt
          array-div-left array-div-right
          array+ array+! array- array-!
          array* array*! array/ array/!
          array-map-elements array-map-elements!
          array-exp array-exp! array-log array-log!
          array-expt array-expt! array-square array-square!
          array-min! array-max! array-min array-max
          array-relu array-dot array-convolve
          array-sum array-1norm array-2norm
          array-inf-norm array-norm array-max-norm
          array-mean array-rows array-columns
          array-select-columns array-unselect-columns
          array-diag array-sum-axis array-sum-axis/squeeze
          array-sum-rows array-normalize-rows
          pretty-print-array)
  ;; old names for backwards compatibility
  (export (rename array* array-mul-elements)
          (rename array/ array-div-elements)
          (rename array+ array-add-elements)
          (rename array- array-sub-elements)
          (rename array*! array-mul-elements!)
          (rename array/! array-div-elements!)
          (rename array+! array-add-elements!)
          (rename array-! array-sub-elements!))
  (include "linalg.scm")
  (cond-expand
   ((and chibi (not no-ffi))
    (import (srfi 160 base) (srfi 231 base))
    (include-shared "blas")
    (begin
      (define (storage->zero storage)
        (cond ((eq? storage c64-storage-class) (make-c64vector 1 0.0))
              ((eq? storage c128-storage-class) (make-c128vector 1 0.0))
              ((eq? storage f64-storage-class) 0.0)
              ((eq? storage f32-storage-class) 0.0)
              (else 0)))
      (define (storage->unit storage)
        (cond ((eq? storage c64-storage-class) (make-c64vector 1 1.0))
              ((eq? storage c128-storage-class) (make-c128vector 1 1.0))
              ((eq? storage f64-storage-class) 1.0)
              ((eq? storage f32-storage-class) 1.0)
              (else 1)))
      (define storage->unit-vec
        (let ((f32-unit (make-f32vector 1 1.0))
              (f64-unit (make-f64vector 1 1.0))
              (c64-unit (make-c64vector 1 1.0))
              (c128-unit (make-c128vector 1 1.0)))
          (lambda (storage)
            (cond ((eq? storage f32-storage-class) f32-unit)
                  ((eq? storage f64-storage-class) f64-unit)
                  ((eq? storage c64-storage-class) c64-unit)
                  ((eq? storage c128-storage-class) c128-unit)
                  (else #f)))))
      (define storage->neg-unit-vec
        (let ((f32-unit (make-f32vector 1 -1.0))
              (f64-unit (make-f64vector 1 -1.0))
              (c64-unit (make-c64vector 1 -1.0))
              (c128-unit (make-c128vector 1 -1.0)))
          (lambda (storage)
            (cond ((eq? storage f32-storage-class) f32-unit)
                  ((eq? storage f64-storage-class) f64-unit)
                  ((eq? storage c64-storage-class) c64-unit)
                  ((eq? storage c128-storage-class) c128-unit)
                  (else #f)))))
      (define (scalar->uvector x storage)
        (cond ((eq? storage c64-storage-class) (c64vector x))
              ((eq? storage c128-storage-class) (c128vector x))
              (else x)))
      (define (uvector->scalar x)
        (cond ((c64vector? x) (c64vector-ref x 0))
              ((c128vector? x) (c128vector-ref x 0))
              (else x)))
      (define (storage->gemm storage)
        (cond ((eq? storage f32-storage-class) cblas-sgemm)
              ((eq? storage f64-storage-class) cblas-dgemm)
              ((eq? storage c64-storage-class) cblas-cgemm)
              ((eq? storage c128-storage-class) cblas-zgemm)
              (else #f)))
      (define (storage->scal storage)
        (cond ((eq? storage f32-storage-class) cblas-sscal)
              ((eq? storage f64-storage-class) cblas-dscal)
              ((eq? storage c64-storage-class) cblas-cscal)
              ((eq? storage c128-storage-class) cblas-zscal)
              (else #f)))
      (define storage->add-info
        (let ((info (map
                     (lambda (ls)
                       (append ls (list (storage->unit (car ls))
                                        (storage->unit-vec (car ls)))))
                     `((,f32-storage-class ,cblas-saxpy)
                       (,f64-storage-class ,cblas-daxpy)
                       (,c64-storage-class ,cblas-caxpy)
                       (,c128-storage-class ,cblas-zaxpy)))))
          (lambda (storage)
            (cond ((assq storage info) => cdr)
                  (else #f)))))
      (define storage->sub-info
        (let ((info (map
                     (lambda (ls)
                       (append ls (list (scalar->uvector -1.0 (car ls))
                                        (storage->neg-unit-vec (car ls)))))
                     `((,f32-storage-class ,cblas-saxpy)
                       (,f64-storage-class ,cblas-daxpy)
                       (,c64-storage-class ,cblas-caxpy)
                       (,c128-storage-class ,cblas-zaxpy)))))
          (lambda (storage)
            (cond ((assq storage info) => cdr)
                  (else #f)))))
      (define (storage->dot storage)
        (cond ((eq? storage f32-storage-class) cblas-sdot)
              ((eq? storage f64-storage-class) cblas-ddot)
              (else #f)))
      (define (specialized-array-mul! alpha a b beta c)
        (let* ((M (interval-width (array-domain a) 0))
               (N (interval-width (array-domain b) 1))
               (K (interval-width (array-domain a) 1))
               (lda (vector-ref (array-coeffs a) 1))
               (ldb (vector-ref (array-coeffs b) 1))
               (ldc (vector-ref (array-coeffs c) 1)))
          (assert (= K (interval-upper-bound (array-domain b) 0)))
          ((storage->gemm (array-storage-class a))
           CblasRowMajor CblasNoTrans CblasNoTrans
           M N K
           alpha (array-body a) lda
           (array-body b) ldb
           beta (array-body c) ldc)
          c))
      (define (array-mul2 a b)
        (if (and (specialized-array? a) (specialized-array? b)
                 (eq? (array-storage-class a) (array-storage-class b))
                 (storage->gemm (array-storage-class a))
                 (zero? (vector-ref (array-coeffs a) 0))
                 (= 1 (vector-ref (array-coeffs a) 2))
                 (zero? (vector-ref (array-coeffs b) 0))
                 (= 1 (vector-ref (array-coeffs b) 2)))
            (let* ((M (interval-upper-bound (array-domain a) 0))
                   (N (interval-upper-bound (array-domain b) 1))
                   (storage (array-storage-class a))
                   (res (make-specialized-array (make-interval (vector M N))
                                                storage)))
              (specialized-array-mul! (storage->unit storage) a b
                                      (storage->zero storage) res))
            (general-array-mul2 a b)))
      (define (array-mul! c a b . o)
        (let* ((storage (array-storage-class c))
               (alpha (if (pair? o)
                          (scalar->uvector (car o) storage)
                          (storage->unit storage)))
               (beta (if (and (pair? o) (pair? (cdr o)))
                         (scalar->uvector (cadr o) storage)
                         (storage->zero storage))))
          (if (and (specialized-array? a) (specialized-array? b)
                   (eq? storage (array-storage-class a))
                   (eq? storage (array-storage-class b))
                   (storage->gemm storage)
                   (zero? (vector-ref (array-coeffs a) 0))
                   (= 1 (vector-ref (array-coeffs a) 2))
                   (zero? (vector-ref (array-coeffs b) 0))
                   (= 1 (vector-ref (array-coeffs b) 2)))
              (specialized-array-mul! alpha a b beta c)
              (general-array-mul! (uvector->scalar alpha) a b
                                  (uvector->scalar beta) c))))
      (define (vectorizable-array? a)
        (and (specialized-array? a)
             (or (= 1 (array-dimension a))
                 (array-packed? a))
             (zero? (vector-ref (array-coeffs a) 0))))
      (define (array-vectorized-step a)
        (vector-ref (array-coeffs a) (- (vector-length (array-coeffs a)) 1)))
      (define (fast-array-inc! a b get-info)
        (cond
         ((and (vectorizable-array? a)
               (get-info (array-storage-class a)))
          =>
          (lambda (info)
            (let ((axpy (car info))
                  (unit (cadr info))
                  (unit-vec (car (cddr info)))
                  (volume (interval-volume (array-domain a)))
                  (a-step (array-vectorized-step a)))
              (cond ((not (array? b))
                     ;; a = b*unit + a
                     (axpy volume (inexact b) unit-vec 0 (array-body a) a-step)
                     a)
                    ((and (vectorizable-array? b)
                          (eq? (array-storage-class a)
                               (array-storage-class b))
                          ;; TODO: broadcasting in blas
                          (equal? (array-domain a)
                                  (array-domain b)))
                     ;; a = unit*b + a
                     (axpy volume unit
                           (array-body b)
                           (array-vectorized-step b)
                           (array-body a)
                           a-step)
                     a)
                    (else
                     #f)))))
         (else
          #f)))
      (define (fast-array+! a b)
        (fast-array-inc! a b storage->add-info))
      (define (fast-array-! a b)
        (fast-array-inc! a b storage->sub-info))
      (define (fast-array*! a b)
        (cond
         ((and (vectorizable-array? a)
               (not (array? b))
               (storage->scal (array-storage-class a)))
          =>
          (lambda (scal)
            (let ((volume (interval-volume (array-domain a)))
                  (a-step (array-vectorized-step a)))
              (scal volume (inexact b) (array-body a) a-step)
              a)))
         (else
          #f)))
      (define (fast-array/! a b)
        (cond
         ((and (vectorizable-array? a)
               (not (array? b))
               (storage->scal (array-storage-class a)))
          =>
          (lambda (scal)
            (let ((volume (interval-volume (array-domain a)))
                  (a-step (array-vectorized-step a)))
              (scal volume (/ (inexact b)) (array-body a) a-step)
              a)))
         (else
          #f)))
      (define (array-dot a b)
        (cond
         ((and (vectorizable-array? a)
               (vectorizable-array? b)
               (eq? (array-storage-class a) (array-storage-class b))
               (storage->dot (array-storage-class a)))
          => (lambda (dot)
               (dot (interval-volume (array-domain a))
                    (array-body a) (vector-ref (array-coeffs a) 1)
                    (array-body b) (vector-ref (array-coeffs b) 1))))
         (else (general-array-dot a b))))
      ))
   (else
    (begin
      (define array-mul2 general-array-mul2)
      (define (array-mul! c a b . o)
        (let ((alpha (if (pair? o) (car o) 1))
              (beta (if (and (pair? o) (pair? (cdr o))) (cadr o) 0)))
          (general-array-mul! alpha a b beta c)))
      (define (fast-array+! a b) #f)
      (define (fast-array-! a b) #f)
      (define (fast-array*! a b) #f)
      (define (fast-array/! a b) #f)
      (define array-dot general-array-dot)
      ))))
