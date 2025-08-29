
(define-library (chibi finance currency)
  (import (scheme base) (srfi 227) (chibi assert))
  (export currency? currency-name currency-code
          currency-symbol currency-subunit currency-precision
          symbol->currency currency-amount->short-string
          USD EUR GBP JPY)
  (include "currency.scm"))
