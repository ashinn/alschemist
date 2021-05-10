
(define-library (chibi math linalg)
  (import (scheme base) (scheme list) (scheme write)
          (srfi 33) (srfi 160 base) (srfi 179) (srfi 179 base)
          (chibi assert) (chibi optional))
  (export array= array-concatenate identity-array
          array-inverse determinant
          array-mul array-expt array-div-left array-div-right
          array-add-elements! array-sub-elements!
          array-mul-elements! array-div-elements!
          pretty-print-array)
  (include "linalg.scm")
  (cond-expand
   ((and chibi (not no-ffi))
    (include-shared "blas")
    (begin
      (define (storage->unit storage)
        (cond ((eq? storage c64-storage-class) (make-c64vector 1 1.0))
              ((eq? storage c128-storage-class) (make-c128vector 1 1.0))
              (else 1.0)))
      (define (storage->gemm storage)
        (cond ((eq? storage f32-storage-class) cblas-sgemm)
              ((eq? storage f64-storage-class) cblas-dgemm)
              ((eq? storage c64-storage-class) cblas-cgemm)
              ((eq? storage c128-storage-class) cblas-zgemm)
              (else #f)))
      (define (specialized-array-mul a b)
        (let* ((M (interval-upper-bound (array-domain a) 0))
               (N (interval-upper-bound (array-domain b) 1))
               (K (interval-upper-bound (array-domain a) 1))
               (lda (vector-ref (array-coeffs a) 1))
               (ldb (vector-ref (array-coeffs b) 1))
               (ldc M)
               (storage (array-storage-class a))
               (unit (storage->unit storage)))
          (assert (= K (interval-upper-bound (array-domain b) 0)))
          (let ((c (make-specialized-array (make-interval (vector M N))
                                           storage)))
            ((storage->gemm storage)
             CblasRowMajor CblasNoTrans CblasNoTrans
             M N K
             unit (array-body a) lda
             (array-body b) ldb
             unit (array-body c) ldc)
            c)))
      (define (array-mul a b)
        (if (and (specialized-array? a) (specialized-array? b)
                 (eq? (array-storage-class a) (array-storage-class b))
                 (storage->gemm (array-storage-class a))
                 (zero? (vector-ref (array-coeffs a) 0))
                 (= 1 (vector-ref (array-coeffs a) 2))
                 (zero? (vector-ref (array-coeffs b) 0))
                 (= 1 (vector-ref (array-coeffs b) 2)))
            (specialized-array-mul a b)
            (general-array-mul a b)))
      ))
   (else
    (begin
      (define array-mul general-array-mul)))))
