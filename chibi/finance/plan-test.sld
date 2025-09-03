
(define-library (chibi finance plan-test)
  (import (scheme base)
          (scheme list)
          (chibi chrono)
          (chibi chrono schedule)
          (chibi finance asset)
          (chibi finance plan)
          (chibi test))
  (export run-tests)
  (begin
    (define (@ str) (string->temporal str))
    (define (portfolio-round-values-by-type pf)
      (map (lambda (x) (cons (car x) (exact (round (cdr x)))))
           (portfolio-values-by-type pf)))
    (define (run-tests)
      (parameterize ((finance-offline-data? #t))
        (test-group "(chibi finance plan)"
          (run-group-tests))))
    (define (run-group-tests)
      ;; stuff $100/month into your mattress
      (test '((liquid . 36000))
          (portfolio-values-by-type
           (fplan-project
            (financial-plan
             (@ "2030-01-01")
             (monthly (net-income 100))
             (yearly passive-gains))
            (@ "2060-01-01"))))
      ;; save $100/month in a bank account with 3% interest
      (test '((liquid . 57226))
          (portfolio-round-values-by-type
           (fplan-project
            (financial-plan
             (@ "2030-01-01")
             (bank-account "MyBank" 0. 'USD 0.03)
             (monthly (net-income 100))
             (yearly passive-gains))
            (@ "2060-01-01"))))
      ;; note the range
      (test-assert (< (* 100 12 30) 57233 (* 100 12 30 (expt 1.03 30))))
      ;; a little extra - the algorithm above deposits monthly but
      ;; only accrues YoY interest, whereas future-value accrues
      ;; interest every period.
      (test 58273.688 (future-value .03 12 30 100))
      ;; very slightly less, off by a month?
      (test 57090. (future-value .03 1 30 1200))
      ;; the above plan, explicitly referring to the account by name
      (test '((liquid . 57226))
          (portfolio-round-values-by-type
           (fplan-project
            (financial-plan
             (@ "2030-01-01")
             (bank-account "OtherBank" 0. 'USD 0.01)
             (bank-account "MyBank" 0. 'USD 0.03)
             (monthly (net-income 100 'USD "MyBank"))
             (yearly passive-gains))
            (@ "2060-01-01"))))
      ;; invest $100/month in stock with 5% cagr
      (let ((AAPL (make-stock 'AAPL 230. 'USD 0. 0.05)))
        (test '((liquid . 100)
                (stock . 80038))
            (portfolio-round-values-by-type
             (fplan-project
              (financial-plan
               (@ "2030-01-01")
               (brokerage-account
                "MyBroker"
                (list (make-asset "Apple" 0 AAPL)
                      (make-asset "Cash" 100 'USD)))
               (monthly (net-income 100 'USD))
               (monthly (buy-stock AAPL 100 'USD))
               (yearly passive-gains))
              (@ "2060-01-01")))))
      ;; invest $100/month in stock with 5% cagr and 2% dividends
      (let ((AAPL (make-stock 'AAPL 230. 'USD 0.02 0.05)))
        (test '((liquid . 18596)
                (stock . 80038))
            (portfolio-round-values-by-type
             (fplan-project
              (financial-plan
               (@ "2030-01-01")
               (brokerage-account
                "MyBroker"
                (list (make-asset "Apple" 0 AAPL)
                      (make-asset "Cash" 100 'USD)))
               (monthly (net-income 100 'USD))
               (monthly (buy-stock AAPL 100 'USD))
               (yearly passive-gains))
              (@ "2060-01-01")))))
      ;; invest $100/month in stock with 5% cagr and 2% dividends,
      ;; reinvesting dividends, first time reaching >$100k
      (let ((AAPL (make-stock 'AAPL 230. 'USD 0.02 0.05)))
        (test '((liquid . 100)
                (stock . 115413))
            (portfolio-round-values-by-type
             (fplan-project
              (financial-plan
               (@ "2030-01-01")
               (brokerage-account
                "MyBroker"
                (list (make-asset "Apple" 0 AAPL)
                      (make-asset "Cash" 100 'USD)))
               (monthly (net-income 100 'USD))
               (monthly (buy-stock/cash-reserves AAPL 100 'USD))
               (yearly passive-gains))
              (@ "2060-01-01")))))
      ;; let's be more aggressive and assume cagr 17% for S&P 500
      (test 776926.941 (future-value .17 1 30 1200))
      ;; $100/month is not huge, let's work another 2 years to break $1m
      (test 1066139.29 (future-value .17 1 32 1200))
      ;; invest a more aggressive $250/month to retire early
      (test 2665348.225 (future-value .17 1 32 3000))
      (test 2375000.0 (fire-number 95000))
      (test 32
          (years-to-fire (lambda (years) (future-value .17 1 years 3000))
                         95000))
      )))
