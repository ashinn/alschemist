
(define-library (chibi plot)
  (import (scheme base) (scheme inexact) (scheme list)
          (scheme process-context) (scheme write)
          (srfi 231)
          (chibi io) (chibi math stats) (chibi process))
  (export-all)
  (include "plot.scm"))
