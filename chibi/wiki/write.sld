(define-library (chibi wiki write)
  (import (scheme base) (scheme cxr) (srfi 1)
          (chibi string) (chibi uri) (chibi sxml) (chibi wiki utils))
  (export wiki-write)
  (include "write.scm"))
