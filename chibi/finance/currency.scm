
(define-record-type Currency
  (make-currency name code symbol subunit precision)
  currency?
  (name currency-name)
  (code currency-code)
  (symbol currency-symbol)
  (subunit currency-subunit)
  (precision currency-precision))

(define USD (make-currency "US dollar" 'USD "$" "cent" 2))
(define EUR (make-currency "euro" 'EUR "€" "cent" 2))
(define JPY (make-currency "Japan yen" 'JPY "¥" #f 0))
(define GBP (make-currency "British pound" 'GBP "£" "pence" 2))

(define *currencies*
  '((USD . ,USD)
    (EUR . ,EUR)
    (JPY . ,JPY)
    (GBP . ,GBP)))

(define (symbol->currency sym)
  (cond ((assq sym *currencies*) => cdr)
        (else #f)))

(define current-currency (make-parameter 'USD))

(define (generic->short-string amount symbol)
  (cond
   ((negative? amount)
    (string-append "-" (generic->short-string (abs amount) symbol)))
   ((< amount 1000)
    (string-append symbol (number->string (exact (round amount)))))
   ((< amount 1000000)
    (string-append symbol
                   (number->string (exact (round (/ amount 1000.))))
                   "k"))
   (else
    (string-append symbol
                   (number->string (exact (round (/ amount 1000000.))))
                   "m"))))

(define (jpy->short-string yen)
  (cond
   ((negative? yen)
    (string-append "-" (jpy->short-string (abs yen))))
   ((< yen 10000)
    (string-append (number->string (exact (round yen))) "円"))
   ((< yen 100000000)
    (string-append (number->string (exact (round (/ yen 10000.)))) "万円"))
   (else
    (string-append
     (number->string (exact (floor (/ yen 100000000.))))
     "億"
     (jpy->short-string (modulo (exact (round yen)) 100000000))))))

;; Just for simple reporting right now, should make this more generic,
;; internationalize, and probably integrate with (chibi show).
(define currency-amount->short-string
  (opt-lambda (amount (currency (current-currency)))
    (let ((currency (if (symbol? currency)
                        (symbol->currency currency)
                        currency)))
      (assert (currency? currency))
      (if (eq? currency JPY)
          (jpy->short-string amount)
          (generic->short-string amount
                                 (currency-symbol currency)
                                 (currency-precision currency))))))
