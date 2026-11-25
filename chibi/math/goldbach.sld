
(define-library (chibi math goldbach)
  (import (scheme small) (srfi 160 base) (chibi assert) (chibi math prime))
  (export as-sum-of-2-primes
          check-sum-of-2-primes)
  (include "goldbach.scm"))
