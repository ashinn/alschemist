
;; Burrows–Wheeler transform, a reversible sort.

(define-library (chibi coding bwt)
  (export block-sort invert-block-sort)
  (import (scheme base) (scheme list) (srfi 95) (srfi 130))
  (include "bwt.scm"))
