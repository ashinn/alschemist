
(define-library (chibi math stats)
  (import (scheme base)
          (scheme inexact)
          (scheme write)
          (scheme comparator)
          (scheme flonum)
          (scheme hash-table)
          (scheme list)
          (scheme mapping)
          (scheme set)
          (scheme vector)
          (only (scheme sort) vector-find-median vector-separate!)
          (srfi 27)
          (srfi 95) ;; we want generic sort with a key
          (chibi optional)
          )
  (export
   ;; distributions
   distribution? distribution-open? distribution-pure?
   distribution-discrete? distribution-population? distribution-add!
   distribution-pdf distribution-cdf
   distribution population summary-distribution open-distribution
   pure-discrete-distribution pure-distribution
   summarize mean-distribution bernoulli-trials
   binomial-distribution kolmogorov-distribution
   poisson-distribution discrete-uniform-distribution
   uniform-distribution normal-distribution f-distribution
   distribution-name distribution-pdf distribution-cdf
   distribution-random distribution-population

   ;; descriptive stats
   size
   mean geometric-mean harmonic-mean quadratic-mean
   mode median
   stdev standard-deviation variance covariance
   kurtosis excess-kurtosis skewness
   coefficient-of-variation
   percentile minimum maximum
   pearson-correlation spearman-rank-correlation
   gain

   ;; hypothesis testing
   z-test z-test? z-statistic
   t-test t-test? t-df t-statistic
   chi^2-test chi^2-test? chi^2-df chi^2-statistic
   g-test g-test? g-statistic
   binomial-test binomial-test?
   kolmogorov-smirnoff-test kolmogorov-smirnoff-test?
   kolmogorov-smirnoff-critical kolmogorov-smirnoff-statistic
   f-test f-test? f-statistic
   wilcoxon-test? wilcoxon-statistic wilcoxon-critical

   ;; sampling
   min-sample-size random-multi-sample make-multi-sampler
   random-sample random-pick flip?

   ;; misc
   sorenson-dice-index jaccard-distance jaccard-index
   wilson-upper-bound wilson-lower-bound wilson-score-offset
   permutations combinations

   ;; data structures
   ;;; histograms
   histogram-cdf histogram-plot-ascii
   histogram-deciles histogram-quantile histogram-scale histogram-max
   histogram-min vector->histogram list->histogram
   histogram-add! make-histogram histogram?
   histogram-bins histogram-bins-set! histogram-max-bins histogram-count
   histogram-count-set! reservoir->vector! reservoir-maybe-add!
   ;;; reservoirs
   make-reservoir reservoir? reservoir-elements reservoir-elements-set!
   reservoir-max-size reservoir-count reservoir-count-set!
   ;;; stable running sums
   running-sum-dec! running-sum-inc! running-sum-get make-running-sum
   running-sum? running-sum-sum running-sum-sum-set!

   ;; low-level utilities
   product factorial sign cube tesseract
   log-sum reciprocal-sum square-sum sum map-sum
   map-rank rank
   numeric-diff inverse-transform-cdf inverse-transform-random
   beta beta-pdf lower-incomplete-gamma loggamma

   ;; regression
   fit-line)
  (include "stats.scm"))
