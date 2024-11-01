
(define-library (chibi table)
  (import (scheme base)
          (scheme list)
          (scheme vector)
          (srfi 227)
          (srfi 231))
  (export table table? table-ref table-set!
          table-num-rows table-num-columns table-shape
          table-arrays table-types table-labels)
  (include "table.scm"))
