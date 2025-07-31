
(define-library (chibi math dual)
  (import (scheme base) (scheme list) (srfi 231))
  (export Dual dual as-dual const dual?
          dual-value dual-link dual-label dual-const?
          dual-with-label dual-format-label
          let-duals let*-duals)
  (include "dual.scm"))
