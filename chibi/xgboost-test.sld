
(define-library (chibi xgboost-test)
  (import (scheme base)
          (scheme write)
          (srfi 160 base)
          (chibi xgboost)
          (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "(chibi xgboost)")
      (test-assert #t)
      (test-assert
        (dmatrix? (xgb-create-matrix '#f32(0 1 2 3 1.0 0.0 0.0 1.0) 4 2 -1.0)))
      (test-end))))
