
(define-library (chibi math debug)
  (import (scheme base) (srfi 166 base) (chibi log) (chibi math show-array))
  (export dbg info)
  (begin
    (define-syntax dbg
      (syntax-rules ()
        ((dbg arg0 arg ...)
         (log-debug (with ((writer written-array)
                           (precision 4))
                      arg0
                      arg ...)))))
    (define-syntax info
      (syntax-rules ()
        ((info arg0 arg ...)
         (log-info (with ((writer written-array)
                          (precision 4))
                     arg0
                     arg ...)))))))
