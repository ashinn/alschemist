
(define (compound-interest amount rate period years)
  (if (infinite? period)
      (* amount (exp (* rate years)))
      (* amount (expt (+ 1 (/ rate period))
                      (* period years)))))

;; Start with this if your plan is simple, you only need the rest of
;; the library if factors change over time.
(define future-value
  (opt-lambda (rate period years contribution (initial-amount 0))
    ;; FV = P(1 + r/n)^(nt) + PMT * [((1 + r/n)^(nt) - 1) / (r/n)]
    (+ (compound-interest initial-amount rate period years)
       (* contribution
          (/ (- (expt (+ 1 (/ rate period))
                      (* period years))
                1)
             (/ rate period))))))

;; The amount you need to support yourself for the given annual
;; expenses (subtracting pension etc. as needed).
(define fire-number
  (opt-lambda (annual-expenses (withdrawl-rate .04))
    (assert (< 0. withdrawl-rate 1.))
    (/ annual-expenses withdrawl-rate)))

(define years-to-fire
  (opt-lambda (get-future-value annual-expenses (withdrawl-rate .04))
    (let ((target-amount (fire-number annual-expenses withdrawl-rate)))
      (let lp ((i 0))
        (if (>= (get-future-value i) target-amount)
            i
            (lp (+ i 1)))))))

;; Financial planning tracks the users' portfolio over time, taking
;; into account income, expenses, interest and other life events that
;; impact their assets.

(define-record-type Financial-Plan
  (make-financial-plan portfolio time-table)
  financial-plan?
  (portfolio fplan-portfolio fplan-portfolio-set!)
  (time-table fplan-time-table fplan-time-table-set!))

(define (fplan-add! fplan x)
  (cond
   ((or (portfolio? x) (asset? x))
    (portfolio-add-asset! (fplan-portfolio fplan) x))
   ((schedule? x)
    (time-table-add! (fplan-time-table fplan) x))
   (else
    (error "expected a schedule or asset" x))))

;; The fundamental iterator.  Similar to a time-table, the
;; scheduled objects should be procedures called as
;; \scheme{(proc date portfolio)}.
(define fplan-fold
  (lambda (fplan until kons knil)
    (time-table-project (fplan-time-table fplan)
                        until
                        (lambda (date proc acc)
                          (if proc
                              (kons date
                                    (proc date (fplan-portfolio fplan))
                                    acc)
                              acc))
                        knil)))

(define fplan-map
  (lambda (fplan until proc)
    (reverse (fplan-fold fplan until (lambda (date pf ls) (cons (proc pf) ls)) '()))))

(define fplan-for-each
  (lambda (fplan until proc)
    (fplan-fold fplan until (lambda (date pf res) (proc date pf) res) (if #f #f))))

;; Runs to the given date, optionally printing a summary, and returns
;; the resulting portfolio.
(define fplan-project
  (opt-lambda (fplan until (print? #f))
    (let-values
        (((portfolio tt)
          (fplan-fold fplan
                      until
                      (lambda (date pf ignored)
                        (when print?
                          (write (portfolio-values-by-type pf))
                          (newline))
                        pf)
                      (fplan-portfolio fplan))))
      (fplan-portfolio fplan))))

(define once
  (opt-lambda (proc (when #f) (name "once"))
    (make-one-time-schedule name proc when)))

(define daily
  (opt-lambda (proc (start #f) (end #f) (name "daily"))
    (make-duration-schedule name (make-duration '((day . 1))) proc start end)))

(define weekly
  (opt-lambda (proc (start #f) (end #f) (name "weekly"))
    (make-duration-schedule name (make-duration '((week . 1))) proc start end)))

(define monthly
  (opt-lambda (proc (start #f) (end #f) (name "monthly"))
    (make-duration-schedule name (make-duration '((month . 1))) proc start end)))

(define quarterly
  (opt-lambda (proc (start #f) (end #f) (name "quarterly"))
    (make-duration-schedule name (make-duration '((month . 3))) proc start end)))

(define yearly
  (opt-lambda (proc (start #f) (end #f) (name "yearly"))
    (make-duration-schedule name (make-duration '((year . 1))) proc start end)))

(define net-income
  (opt-lambda (amount (currency (current-currency)) (asset-to-inc #f))
    (lambda (date portfolio)
      (cond
       ((and asset-to-inc
             (find (lambda (asset) (equal? asset-to-inc (asset-name asset)))
                   (portfolio-assets portfolio)))
        => (lambda (asset) (asset-inc! asset amount currency)))
       (else
        (portfolio-inc! portfolio amount currency))))))

(define pension net-income)

(define (expenses amount . o)
  (apply net-income (- amount) o))

(define buy-stock/shares
  (opt*-lambda (stock shares (currency (stock-currency stock)))
    (lambda (date portfolio)
      (let ((amount (* shares
                       (stock-price stock)
                       (fx (stock-currency stock) currency))))
        (portfolio-inc! portfolio (- amount) currency)
        (portfolio-inc! portfolio shares stock)))))

(define buy-stock
  (opt*-lambda (stock amount (currency (stock-currency stock)))
    (let ((shares (/ (* amount (fx currency (stock-currency stock)))
                     (stock-price stock))))
      (buy-stock/shares stock shares currency))))

(define buy-stock/cash-reserves
  (opt*-lambda (stock
                retain
                (currency (stock-currency stock))
                (from-asset #f))
    (lambda (date portfolio)
      (cond
       (from-asset
        (let* ((available (asset-value-in currency))
               (amount (- available retain)))
          (when (positive? amount)
            (let ((shares (/ (* amount (fx currency (stock-currency stock)))
                             (stock-price stock))))
              (asset-inc! from-asset (- amount) currency)
              (portfolio-inc! portfolio shares stock)))))
       (else
        (let* ((available (portfolio-value-by-type portfolio 'liquid currency))
               (amount (- available retain)))
          (when (positive? amount)
            (let ((shares (/ (* amount (fx currency (stock-currency stock)))
                             (stock-price stock))))
              ((buy-stock/shares stock shares currency) date portfolio)))))))))

(define (sell-stock/shares stock shares)
  (lambda (date portfolio)
    (let ((amount (* shares (stock-price stock))))
      (portfolio-inc! portfolio (- shares) stock)
      (portfolio-inc! portfolio amount (stock-currency stock)))))

(define (sell-stock/any amount currency)
  (lambda (date portfolio)
    (portfolio-liquidate! portfolio amount currency)))

(define sell-stock
  (opt*-lambda (amount (stock #f) (currency (if stock
                                                (stock-currency stock)
                                                (current-currency))))
    (if stock
        (let ((shares (/ (* amount (fx currency (stock-currency stock)))
                         (stock-price stock))))
          (sell-stock/shares stock shares))
        (sell-stock/any amount currency))))

(define interest
  (opt*-lambda (asset (period 1) (rate #f))
    (lambda (date portfolio)
      (let ((rate (or rate (if (stock? (asset-unit asset))
                               (stock-cagr (asset-unit asset))
                               (asset-interest asset)))))
        (when (positive? rate)
          (asset-mul! asset (+ 1 (/ rate period))))))))

(define depreciation
  (opt-lambda (asset rate (min-value 0))
    (assert (not (negative? rate)))
    (lambda (date portfolio)
      (when (> (asset-value asset) min-value)
        (asset-mul/lb! asset (- 1 rate) min-value)))))

(define dividends
  (opt*-lambda (asset (period #f) (dest-asset #f) (currency (current-currency)))
    (lambda (date portfolio)
      (cond
       ((portfolio? asset)
        (for-each
         (lambda (a)
           ((dividends a dest-asset currency) date portfolio))
         (portfolio-assets asset)))
       ((and (asset? asset) (stock? (asset-unit asset)))
        (let ((yield (stock-dividend-yield (asset-unit asset))))
          (when (and yield (positive? yield))
            (let ((amount (* (/ yield period)
                             (asset-value-in asset currency))))
              (portfolio-inc! portfolio amount currency)))))))))

(define passive-gains
  (opt-lambda (date portfolio (period 1))
    (define (gains x)
      (cond
       ((portfolio? x)
        (for-each gains (portfolio-assets x)))
       ((asset? x)
        ((interest x period) date portfolio)
        ((dividends x period) date portfolio))))
    (gains portfolio)))

(define mortgage-payment
  (opt-lambda (amount interest-amount mortgage-asset (bank-asset #f) (currency (current-currency)))
    (lambda (date portfolio)
      (when (negative? (asset-value mortgage-asset))
        (let ((mortgage-asset
               (if (string? mortgage-asset)
                   (portfolio-get-asset portfolio mortgage-asset)
                   mortgage-asset))
              (bank-asset (if (string? bank-asset)
                              (portfolio-get-asset portfolio bank-asset)
                              bank-asset))
              (total (+ amount interest-amount)))
          (if bank-asset
              (asset-inc! bank-asset (- total))
              (portfolio-inc! portfolio (- total) currency))
          (asset-inc! mortgage-asset amount currency))))))

(define bank-account
  (opt-lambda (name
               (initial-value 0)
               (currency (current-currency))
               (interest-rate 0.))
    (make-asset name initial-value currency interest-rate 'liquid)))

(define brokerage-account
  (opt-lambda (name (initial-assets '()))
    (make-portfolio name initial-assets)))

;; Generates a plan from a list of assets, schedules, and nested lists
;; thereof.  This allows utility functions to generate both.
(define (financial-plan . args)
  (let lp ((ls args)
           (assets '())
           (schedules '()))
    (cond
     ((null? ls)
      (make-financial-plan (if (and (= 1 (length assets))
                                    (portfolio? (car assets)))
                               (car assets)
                               (make-portfolio "" (reverse assets)))
                           (time-table (time-line (reverse schedules)))))
     ((or (null? (car ls)) (not (car ls)) (eof-object? (car ls)))
      (lp (cdr ls) assets schedules))
     ((pair? (car ls))
      (lp (append (car ls) (cdr ls)) assets schedules))
     ((or (asset? (car ls)) (portfolio? (car ls)))
      (lp (cdr ls) (cons (car ls) assets) schedules))
     ((or (schedule? (car ls)) (temporal? (car ls)) (duration? (car ls)))
      (lp (cdr ls) assets (cons (car ls) schedules)))
     (else
      (error "unexpected object in financial plan" (car ls))))))
