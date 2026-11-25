
(define-library (chibi ssl-test)
  (import (scheme base) (chibi ssl) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "(chibi ssl)")
      (test-assert (ssl-method? (ssl-method 'sslv3)))
      (test-assert (ssl-context? (ssl-ctx-new 'sslv3 #t)))
      (test-end))))
