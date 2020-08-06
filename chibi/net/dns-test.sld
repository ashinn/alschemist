(define-library (chibi net dns-test)
  (export run-tests)
  (import (scheme base) (chibi test) (chibi net dns))
  (begin
    (define (run-tests)
      (test-begin "dns")
      (test '#u8(1 0 0 1 0 0 0 0 0 0 9 115 121 110 116 104
                 99 111 100 101 3 99 111 109 0 0 1 0 1)
        (bytevector-copy
         (dns-build-query dns/QUERY
                          (list (list "synthcode.com" dns/A dns/IN)))
         2))
      (test '#u8(1 0 0 1 0 0 0 0 0 0 9 115 121 110 116 104
                 99 111 100 101 3 99 111 109 0 0 15 0 1)
        (bytevector-copy
         (dns-build-query dns/QUERY
                          (list (list "synthcode.com" dns/MX dns/IN)))
         2))
      (test-end))))
