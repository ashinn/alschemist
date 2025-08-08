
(define-library (chibi math debug)
  (import (scheme base) (srfi 166 base) (chibi log) (chibi math show-array))
  (export dbg dbg/trace info warn)
  (begin
    (define-syntax dbg/trace
      (syntax-rules ()
        ((dbg/trace arg0 arg ...)
         (log-trace (with ((writer written-array)
                           (array-precision 4))
                      arg0
                      arg ...)))))
    (define-syntax dbg
      (syntax-rules ()
        ((dbg arg0 arg ...)
         (log-debug (with ((writer written-array)
                           (array-precision 4))
                      arg0
                      arg ...)))))
    (define-syntax info
      (syntax-rules ()
        ((info arg0 arg ...)
         (log-info (with ((writer written-array)
                          (array-precision 4))
                     arg0
                     arg ...)))))
    (define-syntax warn
      (syntax-rules ()
        ((info arg0 arg ...)
         (log-warn (with ((writer written-array)
                          (array-precision 4))
                     arg0
                     arg ...)))))))
