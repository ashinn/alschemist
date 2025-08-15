
(define-library (chibi sqlite3 memoize)
  (import (scheme base)
          (scheme list)
          (scheme read)
          (scheme write)
          (scheme time)
          (srfi 98)
          (chibi filesystem)
          (chibi optional)
          (chibi pathname)
          (chibi sqlite3))
  (cond-expand
   (chibi
    (import (only (chibi ast) procedure-arity procedure-variadic?)))
   (else
    (begin
      (define (procedure-arity proc) #f)
      (define (procedure-variadic? proc) #t))))
  (export make-sqlite3-cache sqlite3-cache-ref! memoize/sqlite3
          define-memoized/sqlite3)
  (include "memoize.scm"))
