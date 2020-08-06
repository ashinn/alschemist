
(define-library (chibi term sixel)
  (import (scheme base) (scheme file) (srfi 130) (srfi 151) (scheme write))
  (export write-sixel8)
  (include "sixel.scm"))
