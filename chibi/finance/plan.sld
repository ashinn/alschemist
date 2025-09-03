
(define-library (chibi finance plan)
  (import (scheme base)
          (scheme inexact)
          (scheme list)
          (scheme write)
          (srfi 227)
          (chibi assert)
          (chibi chrono)
          (chibi chrono schedule)
          (chibi finance asset)
          (chibi finance currency))
  (export compound-interest future-value fire-number years-to-fire
          financial-plan financial-plan? fplan-add!
          fplan-fold fplan-map fplan-for-each fplan-project
          net-income pension expenses interest dividends
          buy-stock buy-stock/shares buy-stock/cash-reserves
          sell-stock depreciation mortgage-payment
          once daily weekly monthly quarterly yearly
          bank-account brokerage-account
          passive-gains
          )
  (include "plan.scm"))
