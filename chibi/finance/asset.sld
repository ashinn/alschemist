
(define-library (chibi finance asset)
  (import (scheme base)
          (srfi 1)
          (srfi 98)
          (srfi 227)
          (chibi assert)
          (chibi finance currency)
          (chibi finance quotes)
          (chibi memoize)
          (chibi log))
  (export
   make-stock stock? stock-symbol stock-price stock-currency
   stock-dividend-yield stock-cagr stock-volatility
   stock-price-set! stock-dividend-yield-set! stock-cagr-set!
   stock-shares-for-amount
   make-asset asset? asset-name asset-value asset-type asset-unit
   asset-interest asset-value-in asset-stock? asset-copy
   asset-inc! asset-mul! asset-mul/lb! asset-value-set!
   make-portfolio portfolio? portfolio-copy
   portfolio-name portfolio-assets portfolio-get-asset
   portfolio-value-in portfolio-values->alist
   portfolio-values-by-type portfolio-value-by-type
   portfolio-flat-assets portfolio-add-asset! portfolio-remove-asset!
   portfolio-inc! portfolio-sell! portfolio-buy! portfolio-liquidate!
   finance-offline-data? fx)
  (include "asset.scm"))
