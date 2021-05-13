
(define-library (chibi mecab-test)
  (import (scheme base) (chibi mecab) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "mecab")
      ;; We punt here and just test that the FFI bindings are
      ;; available, since we don't necessarily have a dictionary
      ;; installed.
      (test "" (mecab-last-error))
      (test-end))))
