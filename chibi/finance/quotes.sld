
(define-library (chibi finance quotes)
  (import (scheme base)
          (scheme file)
          (scheme lazy)
          (srfi 98)
          (srfi 130)
          (srfi 227)
          (chibi io)
          (chibi json)
          (chibi log)
          (chibi match)
          (chibi pathname)
          (chibi shell)
          (chibi sqlite3 memoize)
          (chibi temp-file)
          (chibi uri))
  (export get-stock-quote
          get-live-stock-quote
          get-stock-price
          get-exchange-quote
          get-exchange-rate
          get-yahoo-crumb)
  (cond-expand
   ((library (chibi temp-file))
    (import (chibi temp-file))
    (begin
      (define (temp-file template)
        (call-with-temp-file
            template
          (lambda (path out keep!)
            (keep!)
            path)))))
   (else
    (import (srfi 27))
    (begin
      (define (temp-file template)
        (string-append "/tmp/"
                       template
                       (number->string (random-integer 1000000)))))))
  (include "quotes.scm"))
