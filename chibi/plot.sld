
(define-library (chibi plot)
  (import (scheme base) (scheme inexact) (scheme list)
          (scheme process-context) (scheme write)
          (srfi 179)
          (chibi io) (chibi math stats) (chibi math linalg) (chibi process))
  (export-all)
  (include "plot.scm"))
