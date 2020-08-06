(define-library (park random)
  (export random srand)
  (import (scheme r5rs))
  (include "rand2.scm"))
