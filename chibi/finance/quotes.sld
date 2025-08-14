
(define-library (chibi finance quotes)
  (import (scheme base)
          (scheme file)
          (scheme lazy)
          (srfi 130)
          (srfi 227)
          (chibi io)
          (chibi json)
          (chibi log)
          (chibi match)
          (chibi shell)
          (chibi temp-file)
          (chibi uri))
  (export get-stock-quote
          get-stock-price
          get-exchange-quote
          get-exchange-rate
          get-yahoo-crumb)
  (include "quotes.scm"))
