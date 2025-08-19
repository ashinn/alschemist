
(define assq-ref
  (opt-lambda (ls key (default #f))
    (cond ((assq key ls) => cdr)
          (else default))))

;; This env var can be set to use only the latest values specified in
;; the source code instead of looking up live finance data.
(define finance-offline-data?
  (make-parameter
   (cond ((get-environment-variable "FINANCE_OFFLINE_DATA")
          => (lambda (str) (member str '("1" "#t" "true" "TRUE"))))
         (else #f))))

(define-memoized (get-fx-rate from to)
  (get-exchange-rate from to))

;; Shorthand for getting the current exchange rate optionally falling
;; back to a default value.
(define fx
  (opt-lambda (from to (default-rate #f))
    (cond
     ((eq? from to) 1)
     (default-rate
       (if (finance-offline-data?)
           default-rate
           (guard (exn
                   (else
                    (log-warn "error getting exchange rate, using default: "
                              from " -> " to ": " exn)
                    default-rate))
             (get-fx-rate from to))))
     (else
      (get-fx-rate from to)))))

(define-record-type Stock
  (%make-stock symbol price currency dividend-yield cagr volatility)
  stock?
  (symbol stock-symbol)
  (price stock-price stock-price-set!)
  (currency stock-currency)
  (dividend-yield stock-dividend-yield stock-dividend-yield-set!)
  (cagr stock-cagr stock-cagr-set!)
  ;; annualized
  (volatility stock-volatility stock-volatility-set!))

(define make-stock
  (opt-lambda (symbol
               default-price
               (currency 'USD)
               (dividend-yield 0.0)
               (cagr 0.0)
               (volatility 1.0))
    (let ((info (and (not (finance-offline-data?))
                     (guard (exn (else #f))
                       (get-stock-quote symbol)))))
      (%make-stock symbol
                   (or (guard (exn (else #f))
                         (and info (get-stock-price info)))
                       default-price)
                   currency
                   (or (guard (exn (else #f))
                         (and info (get-stock-dividend-yield info)))
                       dividend-yield)
                   cagr
                   volatility))))

(define (unit->string unit)
  (symbol->string (if (stock? unit) (stock-symbol unit) unit)))

(define-record-type Asset
  (%make-asset name value unit interest type)
  asset?
  (name asset-name)
  ;; The numeric value in currency or number of shares.
  (value asset-value asset-value-set!)
  ;; A currency symbol or Stock object.
  (unit asset-unit)
  ;; Non-zero if the asset accrues interest.
  (interest asset-interest)
  ;; A handy designator when summarizing assets, e.g. 'liquid or
  ;; 'stock but can include other values such as 'real-estate or
  ;; 'pension.
  (type asset-type))

(define make-asset
  (opt*-lambda (name
                (value 0)
                (unit 'JPY)
                (interest 0)
                (type (if (stock? unit) 'stock 'liquid)))
    (assert (string? name))
    (assert (number? value))
    (assert (or (symbol? unit) (stock? unit)))
    (assert (number? interest))
    (assert (symbol? type))
    (%make-asset name value unit interest type)))

(define (asset-stock? asset)
  (stock? (asset-unit asset)))

(define (asset-currency asset)
  (if (asset-stock? asset)
      (stock-currency (asset-unit asset))
      (asset-unit asset)))

(define (asset-inc! asset x . o)
  (log-trace `(asset-inc! ,(asset-name asset) ,x) " => "
             (+ x (asset-value asset)) " " o)
  (let ((cur-val (asset-value asset)))
    (asset-value-set! asset (+ x cur-val))
    (if (and (positive? cur-val)
             (negative? (asset-value asset)))
        (log-warn "asset " (asset-name asset) " became negative: "
                  cur-val " => " (asset-value asset) o))))

(define (asset-mul! asset x . o)
  (log-trace `(asset-mul! ,(asset-name asset) ,x) " => "
             (* x (asset-value asset)) " " o)
  (asset-value-set! asset (* x (asset-value asset))))

(define (asset-mul/lb! asset x lb . o)
  (log-trace `(asset-mul/lb! ,(asset-name asset) ,x) " => "
             (* x (asset-value asset)) " " o)
  (asset-value-set! asset (max lb (* x (asset-value asset)))))

(define asset-value-in
  (opt-lambda (asset currency (default-rate #f))
    (if (asset-stock? asset)
        (* (asset-value asset)
           (stock-price (asset-unit asset))
           (fx (stock-currency (asset-unit asset))
               currency
               default-rate))
        (* (asset-value asset)
           (fx (asset-unit asset)
               currency
               default-rate)))))

(define-record-type Portfolio
  (make-portfolio name assets)
  portfolio?
  (name portfolio-name portfolio-name-set!)
  (assets portfolio-assets portfolio-assets-set!))

(define (portfolio-flat-assets pf)
  (append-map (lambda (a)
                (if (portfolio? a)
                    (portfolio-flat-assets a)
                    (list a)))
              (portfolio-assets pf)))

(define (portfolio-add-asset! pf asset)
  (portfolio-assets-set! pf (cons asset (portfolio-assets pf))))

(define (portfolio-remove-asset! pf asset)
  (portfolio-assets-set! pf (delete asset (portfolio-assets pf))))

(define (portfolio-create-asset! pf amount unit)
  (let ((asset
         (make-asset (string-append (portfolio-name pf) ":" (unit->string unit))
                     amount
                     unit)))
    (portfolio-assets-set! pf (cons asset (portfolio-assets pf)))
    asset))

;; If amount is positive, adds that amount to the first asset in the
;; portfolio of the matching unit, creating a new asset as needed.
;;
;; If amount is negative, subtracts that amount from assets of the
;; matching unit in order until the amount is fulfilled.
(define (portfolio-inc! pf amount unit . o)
  (log-trace  "portfolio-inc! " (portfolio-name pf) " " amount " " unit " " o)
  (cond
   ((zero? amount))
   ((positive? amount)
    ;; positive, just add to the first matching asset
    (let ((asset (or (find (lambda (a)
                             (and (or (stock? unit)
                                      (eq? 'liquid (asset-type a)))
                                  (eq? unit (asset-unit a))))
                           (portfolio-flat-assets pf))
                     (portfolio-create-asset! pf 0 unit))))
      (asset-inc! asset amount)
      asset))
   (else
    ;; negative, maybe deduct from multiple assets
    (let lp ((ls (if (symbol? unit)
                     (filter (lambda (asset)
                               (eq? 'liquid (asset-type asset)))
                             (portfolio-flat-assets pf))
                     (portfolio-flat-assets pf)))
             (needed (- amount))
             (res '())
             (liquid-only? (symbol? unit)))
      (cond
       ((and (null? ls) (positive? needed) liquid-only?)
        (lp (portfolio-flat-assets pf) needed res #f))
       ((or (null? ls) (<= needed 0))
        (for-each
         (lambda (x)
           (let ((asset (car x))
                 (val-to-take (cdr x)))
             (asset-inc! asset (- val-to-take))))
         res)
        (when (positive? needed)
          ;; if there were insufficient funds, log the error and
          ;; subtract the amount from the first available asset,
          ;; resulting in a negative asset
          (log-error "portfolio-inc! " (portfolio-name pf)
                     " insufficient funds to deduct " (- amount) " " unit
                     ", " needed " remaining " res)
          (let ((neg-asset
                 (or (find (lambda (a)
                             (eq? (asset-unit a) unit))
                           (portfolio-flat-assets pf))
                     (portfolio-create-asset! pf unit))))
            (asset-inc! neg-asset (- needed)))))
       ((and (eq? unit (asset-unit (car ls)))
             (positive? (asset-value (car ls))))
        (let ((val-to-take (min needed (asset-value (car ls)))))
          (lp (cdr ls)
              (- needed val-to-take)
              (cons (cons (car ls) val-to-take) res)
              liquid-only?)))
       (else
        (lp (cdr ls) needed res liquid-only?)))))))

;; Sells the given number of shares and stores the proceeds in the
;; same portfolio.
(define portfolio-sell!
  (opt*-lambda (pf
                shares
                stock
                (currency (stock-currency stock))
                (default-rate #f))
    (let ((amount (* shares
                     (stock-price stock)
                     (fx (stock-currency stock)
                         currency
                         default-rate))))
      (portfolio-inc! pf (- shares) stock)
      (portfolio-inc! pf amount currency))))

(define portfolio-buy!
  (opt*-lambda (pf
                shares
                stock
                (currency (stock-currency stock))
                (default-rate #f))
    (let ((amount (* shares
                     (stock-price stock)
                     (fx (stock-currency stock)
                         currency
                         default-rate))))
      (portfolio-inc! pf (- amount) currency)
      (portfolio-inc! pf shares stock))))

;; How many shares we'd need to sell to make the given amount in
;; currency, using a default-rate from stock-currency to currency.
(define stock-shares-for-amount
  (opt-lambda (stock amount currency (default-rate #f))
    ;; For now just allow fractional shares.
    (/ amount
       (* (stock-price stock)
          (fx (stock-currency stock) currency default-rate)))))

;; Sells arbitrary stocks to meet reach the desired amount in
;; currency.
(define portfolio-liquidate!
  (opt-lambda (pf amount currency (default-rates '()))
    (let lp ((ls (portfolio-flat-assets pf))
             (needed amount))
      (cond
       ((or (null? ls) (<= needed 0))
        (when (positive? needed)
          (log-error "failed to liquidate " (portfolio-name pf) " for "
                     amount ", " needed " remaining")))
       ((not (and (asset-stock? (car ls))
                  (positive? (asset-value (car ls)))))
        (lp (cdr ls) needed))
       (else
        (let* ((asset (car ls))
               (stock (asset-unit asset))
               (default-rate (assq-ref default-rates (stock-currency stock)))
               (shares-to-sell
                (min (asset-value asset)
                     (stock-shares-for-amount
                      stock needed currency default-rate)))
               (sold-amount
                (* shares-to-sell
                   (stock-price stock)
                   (fx (stock-currency stock)
                       currency
                       default-rate))))
          (log-trace
           "portfolio-liquidate! " (portfolio-name pf) " sell " shares-to-sell
           " shares of " (stock-symbol stock) " out of " (asset-value asset)
           " for " sold-amount " (needed "
           (stock-shares-for-amount stock amount currency default-rate)
           " shares at " (stock-price stock) " " (stock-currency stock)
           " for "  needed ")")
          (when (positive? shares-to-sell)
            (asset-inc! asset (- shares-to-sell))
            (portfolio-inc! pf sold-amount currency))
          (lp (cdr ls) (- needed sold-amount))))))))

(define portfolio-value-in
  (opt-lambda (pf currency (default-rates '()))
    (fold (lambda (asset total)
            (+ total
               (asset-value-in
                asset
                currency
                (assq-ref default-rates (asset-currency asset)))))
          0
          (portfolio-flat-assets pf))))

(define portfolio-values->alist
  (opt-lambda (pf currency (default-rates '()))
    (map (lambda (x)
           (if (portfolio? x)
               (cons (portfolio-name x)
                     (portfolio-value-in x currency default-rates))
               (cons (asset-name x)
                     (asset-value-in x
                                     currency
                                     (assq-ref default-rates
                                               (asset-currency x))))))
         (portfolio-assets pf))))

(define (add-asset-by-type! res asset currency rate)
  (let ((type (asset-type asset)))
    (cond
     ((assq type res)
      => (lambda (cell)
           (set-cdr! cell (+ (cdr cell)
                             (asset-value-in asset currency rate)))
           res))
     (else
      (cons (cons type (asset-value-in asset currency rate)) res)))))

(define portfolio-values-by-type
  (opt-lambda (pf currency (default-rates '()))
    (fold (lambda (asset res)
            (let ((rate (assq-ref default-rates (asset-currency asset))))
              (add-asset-by-type! res asset currency rate)))
          '()
          (portfolio-flat-assets pf))))
