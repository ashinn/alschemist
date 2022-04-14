;; stats.scm -- basic statistics library
;; Copyright (c) 2019-2020 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Statistics is the branch of mathematics dealing with the collection
;;> and analysis of data.  As we are increasingly overwhelmed with data
;;> it is important to be able to analyze that data, and so statistics
;;> should have a prominant role in any programming language's standard
;;> libraries.
;;>
;;> Broadly speaking statistics can be divided into descriptive
;;> statistics and inferential statistics.  This library provides
;;> basic utilities for both of these covering many common use cases,
;;> while serving as a basis for more advanced uses.

(define-syntax assert
  (syntax-rules ()
    ((assert expr ...)
     (if (not (and expr ...))
         (error "assertion failed" '(and expr ...))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence utilities - all procedures handle both lists and vectors

(define (seq? seq)
  (or (pair? seq) (null? seq) (vector? seq)))

(define (seq-length seq)
  (if (vector? seq) (vector-length seq) (length seq)))

(define (seq-ref seq k)
  (if (vector? seq) (vector-ref seq k) (list-ref seq k)))

(define (seq-for-each proc seq . o)
  (apply (if (vector? seq) vector-for-each for-each) proc seq o))

(define (seq-fold kons knil seq . o)
  (if (vector? seq)
      (apply vector-fold kons knil seq o)
      ;; accumulator agrees with SRFI 133 order, not SRFI 1
      (apply fold
             (lambda args (apply kons (last args) (drop-right args 1)))
             knil seq o)))

(define (seq-map proc seq . o)
  (apply (if (vector? seq) vector-map map) proc seq o))

(define (seq-any pred seq . o)
  (if (null? o)
      ((if (vector? seq) vector-any any) pred seq)
      (let* ((seq1 seq)
             (seq2 (car o))
             (len1 (seq-length seq1))
             (len2 (seq-length seq1)))
        (let lp ((i 0) (j 0) (ls1 seq1) (ls2 seq2))
          (and (< i len1)
               (< j len2)
               (or (pred (if (pair? ls1) (car ls1) (seq-ref seq1 i))
                         (if (pair? ls2) (car ls2) (seq-ref seq2 j)))
                   (lp (+ i 1) (+ j 1)
                       (if (pair? ls1) (cdr ls1) ls1)
                       (if (pair? ls2) (cdr ls2) ls2))))))))

(define (seq-every pred seq . o)
  (not (apply seq-any (lambda args (not (apply pred args))) seq o)))

(define (seq-mean seq . o)
  (/ (apply sum seq o)
     (seq-length seq)))

(define (seq-geometric-mean seq . o)
  (expt (apply product seq o)
        (/ (seq-length seq))))

(define (seq-harmonic-mean seq . o)
  (/ (apply seq-length seq o)
     (reciprocal-sum seq)))

(define (seq-quadratic-mean seq . o)
  (sqrt (/ (apply square-sum seq o)
           (seq-length seq))))

(define (seq-median seq . o)
  (if (zero? (seq-length seq))
      (error "median requires non-empty input")
      (let ((less (if (pair? o) (car o) <))
            (mean (if (and (pair? o) (pair? (cdr o)))
                      (cadr o)
                      (lambda (a b) (/ (+ a b) 2)))))
        (vector-find-median less
                            (if (vector? seq) seq (list->vector seq))
                            #f
                            mean))))

(define (seq-mode seq . o)
  (if (zero? (seq-length seq))
      (error "mode requires non-empty input")
      (let* ((cmp (if (pair? o) (car o) the-equal-comparator))
             (ht (make-hash-table cmp))
             (max-elt #f)
             (max-count 0))
        (seq-for-each
         (lambda (elt)
           (let ((new-count (+ 1 (hash-table-ref/default ht elt 0))))
             (hash-table-set! ht elt new-count)
             (when (> new-count max-count)
               (set! max-count new-count)
               (set! max-elt elt))))
         seq)
        max-elt)))

(define (seq-variance seq . o)
  (let ((mu (if (pair? o) (car o) (mean seq))))
    (/ (map-sum (lambda (x) (square (- x mu))) seq)
       (- (seq-length seq) 1))))

(define (seq-pop-variance seq . o)
  (let ((mu (if (pair? o) (car o) (mean seq))))
    (/ (map-sum (lambda (x) (square (- x mu))) seq)
       (seq-length seq))))

(define (seq-stdev seq . o)
  (sqrt (apply seq-variance seq o)))

(define (seq-skew seq . o)
  (let ((mu (apply seq-mean seq o)))
    (/ (map-sum (lambda (x) (cube (- x mu))) seq)
       (seq-length seq)
       (cube (apply seq-stdev seq o)))))

(define (seq-kurtosis seq . o)
  (let ((mu (apply mean seq o)))
    (/ (map-sum (lambda (x) (tesseract (- x mu))) seq)
       (seq-length seq)
       (tesseract (apply standard-deviation seq o)))))

(define (seq-maximum seq . o)
  (let ((less (if (pair? o) (car o) <)))
    (seq-fold (lambda (a b) (if (less a b) b a)) -inf.0 seq)))

(define (seq-minimum seq . o)
  (let ((less (if (pair? o) (car o) <)))
    (seq-fold (lambda (a b) (if (less b a) b a)) +inf.0 seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{Distributions}

;;> This library takes a holistic view of a distribution, with
;;> procedures working on both theoretical and empirical
;;> distributions.

;;> \subsection{Distribution Type}

(define-record-type Reservoir
  (%make-reservoir elements max-size count)
  reservoir?
  (elements reservoir-elements reservoir-elements-set!)
  (max-size reservoir-max-size)
  (count reservoir-count reservoir-count-set!))

(define-record-type Histogram
  (%make-histogram bins max-bins count)
  histogram?
  (bins histogram-bins histogram-bins-set!)
  (max-bins histogram-max-bins)
  (count histogram-count histogram-count-set!))

;; The descriptive statistics.
(define-record-type Statistics
  (make-statistics size mean median mode variance skew kurtosis minimum maximum)
  statistics?
  (size statistics-size statistics-size-set!)
  (mean statistics-mean statistics-mean-set!)
  (median statistics-median statistics-median-set!)
  (mode statistics-mode statistics-mode-set!)
  (variance statistics-variance statistics-variance-set!)
  (skew statistics-skew statistics-skew-set!)
  (kurtosis statistics-kurtosis statistics-kurtosis-set!)
  (minimum statistics-minimum statistics-minimum-set!)
  (maximum statistics-maximum statistics-maximum-set!))

;; A population summary, optionally with a sample which could be
;; running, and histogram.
(define-record-type Summary
  (%make-summary sample sum mean msd density window?)
  summary?
  (sample summary-sample summary-sample-set!)
  (sum summary-sum summary-sum-set!)
  (mean summary-mean summary-mean-set!)
  (msd summary-msd summary-msd-set!)
  (density summary-density summary-density-set!)
  (window? summary-window? summary-window?-set!))

;; Any theoretical, empirical or sampled distribution.
(define-record-type Distribution
  (%make-dist name pdf cdf random population statistics summary discrete? population?)
  distribution?
  (name distribution-name)
  ;; Pure distributions are determined by a pdf, cdf and random function.
  (pdf distribution-pdf)
  (cdf distribution-cdf)
  (random distribution-random)
  ;; The sampled elements for a non-pure distribution.
  (population distribution-population)
  ;; The basic statistics (mean, variance, etc.) of the distribution.
  ;; May be computed lazily for non-pure distributions.
  (statistics distribution-statistics distribution-statistics-set!)
  ;; The summary.
  (summary distribution-summary)
  ;; True iff the distribution is discrete.
  (discrete? distribution-discrete?)
  ;; True iff this is the full population, as opposed to a sample.
  (population? distribution-population?))

(define (make-empty-statistics)
  (make-statistics #f #f #f #f #f #f #f #f #f))

(define (statistics-stdev stats)
  (sqrt (statistics-variance stats)))

(define (summary-count summary)
  (reservoir-count (summary-sample summary)))

(define (make-summary . o)
  (%make-summary (make-reservoir (if (pair? o) (car o) 128))
                 0 0 0 #f (make-histogram) #f))

(define (summary->vector! summary)
  (and (summary-sample summary)
       (reservoir->vector! (summary-sample summary))))

;; Track squared distance from the mean for online variance.
(define (summary-add! summary elt)
  (reservoir-maybe-add! (summary-sample summary) elt)
  (summary-sum-set! summary (+ elt (summary-sum summary)))
  (histogram-add! (summary-density summary) elt)
  (let ((delta (- elt (summary-mean summary))))
    (summary-mean-set! summary (+ (summary-mean summary)
                                  (/ delta (summary-count summary))))
    ;; not delta^2, multiply by diff from prev and new means
    (summary-msd-set! summary (+ (summary-msd summary)
                                 (* delta (- elt (summary-mean summary)))))))

(define (summary-median summary)
  (summary-mean summary)) ;; TODO: compute from sample?

(define (summary-mode summary)
  (summary-mean summary)) ;; TODO: compute from sample?

(define (summary-variance summary)
  (if (<= (summary-count summary) 1)
      0
      (/ (summary-msd summary) (- (summary-count summary) 1))))

(define (summary-stdev summary)
  (sqrt (summary-variance summary)))

;;> \procedure{(distribution? x)}
;;> Returns \scheme{#t} iff x is a distribution.

;;> \procedure{(pure-distribution name pdf cdf statistics [random])}
;;> Creates a new theoretical distribution.  \var{name} should be a
;;> symbol and is used for debugging.  The probability density
;;> function \var{pdf}, cumulative distribution function \var{cdf},
;;> and summary \var{statistics} are required.  \var{random} is
;;> optionally and can be inferred from the \var{cdf} but faster
;;> implementations are usually available.
(define (pure-distribution name pdf cdf statistics . o)
  (let ((random (if (pair? o)
                    (car o)
                    (inverse-transform-random cdf statistics))))
    (%make-dist name pdf cdf random #f statistics #f #f #t)))

;;> Creates a new theoretical discrete distribution, similar to
;;> \scheme{pure-distribution}.
(define (pure-discrete-distribution name pmf cdf statistics . o)
  (let ((random (if (pair? o)
                    (car o)
                    (inverse-transform-random cdf statistics))))
    (%make-dist name pmf cdf random #f statistics #f #t #t)))

;;> Creates a new distribution for the sampled data in \var{seq}.
(define (distribution seq)
  (%make-dist #f #f #f #f seq #f #f (seq-every exact-integer? seq) #f))

;;> Equivalent to \scheme{distribution}, but records that \var{seq}
;;> represents the full population rather than a subsample.
(define (population seq)
  (%make-dist #f #f #f #f seq #f #f (seq-every exact-integer? seq) #t))

;;> Creates a new running sample distribution to which new elements
;;> can be added.
(define (open-distribution)
  (%make-dist #f #f #f #f (make-reservoir) #f #f #f))

;;> Returns a new distribution from summmary statistics, without a
;;> sample.  Takes keyword arguments in the style of
;;> \scheme{(chibi optional)}, with the following values:
;;>
;;> \itemlist[
;;> \item{name: used for debugging}
;;> \item{size: the population size}
;;> \item{mean: the mean (average) value}
;;> \item{median: the median (middle) value}
;;> \item{mode: the mode (most common) value}
;;> \item{variance: the variance}
;;> \item{skew: the skew}
;;> \item{kurtosis: the kurtosis}
;;> \item{minimum: the minimum value}
;;> \item{maximum: the maximum value}
;;> \item{discrete?: true iff this is a discrete distribution}
;;> \item{population?: true iff this is a population (as opposed to sample)}
;;> ]
;;>
;;> Example: create a summary given just a mean and variance:
;;> \example{(mean (summary-distribution 'mean: 0.5 'variance: 2.5))}
(define (summary-distribution . o)
  (let-keywords o ((name #f) (size #f) (mean #f) (median #f) (mode #f)
                   (variance #f) (skew #f) (kurtosis #f)
                   (minimum #f) (maximum #f)
                   (discrete? #f) (population? #f))
    (let ((stats (make-statistics size mean median mode variance
                                  skew kurtosis minimum maximum)))
      (%make-dist name #f #f #f #f stats #f discrete? population?))))

;;> A utility for a summary distribution of bernoulli trials, based on
;;> the number of successes out of the total number of trials.
(define (bernoulli-trials successes trials)
  (assert (<= successes trials))
  (let ((mean (/ successes trials)))
    (summary-distribution 'mean: mean
                          'variance: (* mean (- 1 mean))
                          'size: trials)))

;;> A utility for a summary distribution where we only know the mean.
(define (mean-distribution mean)
  (summary-distribution 'mean: mean))

;;> Returns a new distribution containing only the summary statistics
;;> of the given distribution.
(define (summarize dist)
  ;; TODO: include an option to preserve a subsample summary
  (summary-distribution
   'name: (and (distribution? dist) (distribution-name dist))
   'size: (size dist)
   'mean: (mean dist)
   'median: (median dist)
   'mode: (mode dist)
   'variance: (variance dist)
   'skew: (skewness dist)
   'kurtosis: (kurtosis dist)
   'minimum: (minimum dist)
   'maximum: (maximum dist)
   'discrete?: (if (distribution? dist)
                   (distribution-discrete? dist)
                   (seq-every exact-integer? dist))
   'population?: (and (distribution? dist)
                      (distribution-population? dist))))

;;> Returns \scheme{#t} iff \var{dist1} is a theoretical distribution
;;> (is derived from a pdf).
(define (distribution-pure? dist)
  (and (distribution? dist) (distribution-pdf dist) #t))

;;> Returns \scheme{#t} iff \var{dist1} is a sample distribution to
;;> which new elements can be added.
(define (distribution-open? dist)
  (reservoir? (distribution-population dist)))

;;> Adds a new element \var{elt} to the sample distribution \var{dist}.
(define (distribution-add! dist elt)
  (when (not (distribution-open? dist))
    (error "can't add elements to a fixed distribution" dist elt))
  (summary-add! (distribution-summary dist) elt)
  (if (distribution-statistics dist)
      (distribution-statistics-set! dist #f)))

;;> \procedure{(distribution-pdf dist)}
;;> Returns the pdf if \var{dist} is pure a distribution, \scheme{#f} otherwise.

;;> \procedure{(distribution-cdf dist)}
;;> Returns the cdf if \var{dist} is pure a distribution, \scheme{#f} otherwise.

;;> Returns the underlying values, or sampled representatives, of
;;> \var{dist}, if available.  If you want a sample from any
;;> distribution, use \scheme{random-sample}.
(define (distribution-values dist . o)
  (cond
   ((not (distribution? dist))
    (if (and (pair? o) (car o)) (seq-map (car o) dist) dist))
   ((distribution-population dist))
   ((distribution-summary dist)
    (summary->vector! (distribution-summary dist)))
   (else #f)))

;; In general we can get a stat from the statistics object,
;; from the summary object (for size, mean, variance, stdev),
;; or computed (and then cached) directly from the population
;; or summary elements.
(define (dist-dispatch stats-get stats-set! summary-get seq-get)
  (lambda (dist . o)
    (cond
     ((not (distribution? dist)) (apply seq-get dist o))
     ((and (distribution-statistics dist)
           (stats-get (distribution-statistics dist))))
     (else
      (let ((res (or (and summary-get
                          (distribution-summary dist)
                          (summary-get (distribution-summary dist)))
                     (and seq-get
                          (or (and (distribution-population dist)
                                   (seq-get (distribution-population dist)))
                              (and (distribution-summary dist)
                                   (seq-get
                                    (summary->vector!
                                     (distribution-summary dist)))))))))
        (when (and res stats-set!)
          (when (not (distribution-statistics dist))
            (distribution-statistics-set! dist (make-empty-statistics)))
          (stats-set! (distribution-statistics dist) res))
        res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \subsection{Standard Distributions}

(define (normal-pdf x mu sigma)
  (/ (exp (- (/ (square (- x mu))
                (* 2 (square sigma)))))
     (sqrt (* 2 (acos -1) (square sigma)))))

(define (normal-cdf x mu sigma)
  (/ (+ 1 (flerf (/ (- x mu) (* (sqrt 2) sigma))))
     2))

;; determine z-score critical value
(define inverse-normal-cdf
  (let ((scores '((0.8 . 1.281552)
                  (0.9 . 1.644854)
                  (0.95 . 1.959964)
                  (0.98 . 2.326348)
                  (0.99 . 2.575829)
                  (0.995 . 2.807034)
                  (0.998 . 3.090232)
                  (0.999 . 3.290527)
                  (0.9999 . 3.890592)
                  (0.99999 . 4.417173)
                  (0.999999 . 4.891638)
                  (0.9999999 . 5.326724)
                  (0.99999999 . 5.730729)
                  (0.999999999 . 6.109410))))
    (lambda (percentile)
      (do ((ls scores (cdr ls)))
          ((or (<= percentile (caar ls))
               (null? (cdr ls)))
           (cdar ls))))))

;;> Returns a new normal (gaussian) distribution, with the given
;;> \var{mean} and \var{variance}.  Also know as a bell curve, many
;;> things are normally distributed, and the Central Limit THeorem
;;> says that the sum of independent random variables tends towards
;;> normal.
(define (normal-distribution . o)
  (let* ((mean (if (pair? o) (car o) 0))
         (variance (if (and (pair? o) (pair? (cdr o))) (cadr o) 1))
         (random-source (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                            (car (cddr o))
                            default-random-source))
         (random-real (random-source-make-reals random-source)))
    (pure-distribution
     'normal
     (lambda (x)
       (normal-pdf x mean variance))
     (lambda (x)
       (/ (+ 1 (flerf (/ (- x mean) (* (sqrt 2) variance))))
          2))
     (make-statistics #f mean mean mean variance 0 3 -inf.0 +inf.0)
     (lambda ()
       (+ mean (* variance
                  (sqrt (* -2.0 (log (random-real))))
                  (cos (* 2.0 (acos -1) (random-real)))))))))

;;> Returns a new continuous uniform distribution over [\var{a},
;;> \var{b}].  Every value in the range is equally likely.
(define (uniform-distribution a b . o)
  (assert (<= a b))
  (let* ((mean (/ (+ a b) 2))
         (variance (/ (square (- b a)) 12))
         (diff^-1 (/ (- b a)))
         (random-source (if (pair? o) (car o) default-random-source))
         (random-real (random-source-make-reals random-source)))
    (pure-distribution
     'uniform
     (lambda (x)
       (if (<= a x b) diff^-1 0))
     (lambda (x)
       (cond ((< x a) 0)
             ((< x b) (* diff^-1 (- x a)))
             (else 1)))
     (make-statistics #f mean mean mean variance 0 -6/5 a b)
     (lambda ()
       (+ a (* (random-real) (- b a)))))))

;;> Returns a new discrete uniform distribution over [\var{a},
;;> \var{b}], in \var{step} intervals.  Every value in the range is
;;> equally likely.
(define (discrete-uniform-distribution a b . o)
  (assert (<= a b))
  (let* ((step (if (pair? o) (car o) 1))
         (size (* step (+ 1 (- b a))))
         (n^-1 (/ size))
         (mean (/ (+ a b) 2))
         (variance (/ (- (square (+ 1 (- b a))) 1) 12))
         (kurtosis (* -6/5 (/ (+ 1 (square size)) (+ -1 (square size)))))
         (random-source (if (and (pair? o) (pair? (cdr o)))
                            (cadr o)
                            default-random-source))
         (random-integer (random-source-make-integers random-source)))
    (pure-discrete-distribution
     'uniform
     (lambda (x) n^-1)
     (lambda (x) (/ (+ 1 (- x a)) size))
     (make-statistics #f mean mean #f variance 0 kurtosis a b)
     (lambda () (+ a (* step (random-integer size)))))))

;;> Returns a new poisson distribution, a discrete distribution
;;> measuring the probability that a certain number of events occur
;;> within a certain period of time.
(define (poisson-distribution lam k . o)
  (when (<= lam 0)
    (error "poisson distribution lambda must be positive" lam))
  (unless (and (exact-integer? k) (positive? k))
    (error "poisson distribution k must be a natural number" k))
  (let* ((mean lam)
         (median (+ lam 1/3 (- (/ 0.02 lam))))
         (mode (- (ceiling lam) 1))
         (variance lam)
         (skew (/ (sqrt lam)))
         (kurtosis (/ lam))
         (random-source (if (pair? o) (car o) default-random-source))
         (random-real (random-source-make-reals random-source)))
    (pure-discrete-distribution
     'poisson
     (lambda (x)
       (/ (* (expt lam k) (exp (- lam))) (factorial k)))
     (lambda (x)
       (* (exp (- lam))
          (do ((i 0 (+ i 1))
               (fact 1 (* fact (+ i 1)))
               (sum 0 (+ sum (/ (expt lam i) fact))))
              ((> i k) sum))))
     (make-statistics #f mean median mode variance skew kurtosis 0 k)
     (lambda ()
       (let ((limit (exp (- lam))))  ;; TODO: faster randoms
         (do ((k 0 (+ k 1))
              (p 1 (* p (random-real))))
             ((<= p limit) (- k 1))))))))

;;> Returns a new kolmogorov distribution, the distribution of the
;;> suprememum of the Brownian bridge over [0, 1].  This is used in
;;> the Kolmogorov-Smirnoff test.
(define (kolmogorov-distribution n)
  (let* ((cdf (lambda (x) (kolmogorov-cdf n x)))
         (mean (/ .8687 (sqrt n)))
         (median (cdf .5))
         (variance (/ .26 (sqrt n))))
    (pure-distribution
     'kolmogorov
     (numeric-diff cdf)
     cdf
     (make-statistics #f mean median median variance 0. 0. 0 1))))

;;> Returns a new binomial distribution, a discrete distribution of
;;> the number of successes over \var{n} Bernoulli trials with
;;> probability \var{p}.
(define (binomial-distribution n p)
  (let* ((mean (* n p))
         (median (exact (round mean)))
         (mode (exact (floor (* (+ n 1) p))))
         (variance (* mean (- 1 p)))
         (pmf (lambda (k)
                (* (combinations n k) (expt p k) (expt (- 1 p) (- n k))))))
    (pure-discrete-distribution
     'binomial
     pmf
     (lambda (k)
       (do ((i 0 (+ i 1))
            (s 0 (+ s (pmf i))))
           ((>= i k) s)))
     (make-statistics #f mean median mode variance
                      (/ (- 1 (* 2 p))
                         (sqrt variance))
                      (/ (- 1 (* 6 p (- 1 p)))
                         variance)
                      0
                      n))))

(define (f-pdf x d1 d2)
  (/ (sqrt (/ (* (expt (* d1 x) d1) (expt d2 d2))
              (expt (+ (* d1 x) d2) (+ d1 d2))))
     (* x (beta (/ d1 2) (/ d2 2)))))

(define (f-cdf x d1 d2)
  (beta (/ d1 2)
        (/ d2 2)
        (/ (* d1 x) (+ (* d1 x) d2))))

;;> Returns a new F-distribution, aka the Fisher–Snedecor
;;> distribution, used in the F-test for ANOVA.
(define (f-distribution d1 d2)
  (let ((mean (/ d2 (- d2 2)))
        (mode (/ (* (- d1 2) d2) d1 (+ d2 2)))
        (variance (/ (* 2 (square d2) (+ d1 d2 -2))
                     (* d1 (square (- d2 2)) (- d2 4))))
        (skew (/ (* (+ (* 2 d1) d2 -2) (sqrt (* 8 (- d2 4))))
                 (- d2 6)
                 (sqrt (* d1 (+ d1 d2 -2)))))
        (kurtosis
         (/ (* 12 (+ (* d1 (- (* 5 d2) 22) (+ d1 d2 -2))
                     (* (- d2 4) (square (- d2 2)))))
            (* d1 (- d2 6) (- d2 8) (+ d1 d2 -2)))))
    (pure-distribution
     (lambda (x) (f-pdf x d1 d2))
     (lambda (x) (f-cdf x d1 d2))
     (make-statistics #f mean mean mode variance skew kurtosis 0 +inf.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{Descriptive Statistics}

;;> Descriptive statistics summarize some aspect of a distribution.
;;> The following procedures all take either a distribution, or for
;;> convenience a list or vector as an implicit sample distribution (as
;;> constructed by the \scheme{distribution} procedure).
;;>
;;> If a list of vector is passed, an optional getter argument is
;;> allowed to return the value of each element in the sequence.  This
;;> is to allow idioms similar to dataframes in R and numpy, where the
;;> sequence elements could be vectors or records containing multiple
;;> fields.
;;>
;;> If a distribution is passed, we may not have complete information
;;> for that distribution, so any of these may return \scheme{#f}.

;;> \procedure{(size dist [getter])}
;;> Returns the size of the distribution population, which may be
;;> +inf.0 for pure distributions, or #f if unknown.
;;>
;;> \example{(size '(1 2 3))}
;;> \example{(size (normal-distribution 0 1))}
(define (size dist . o)
  (cond
   ((not (distribution? dist)) (seq-length dist))
   ((distribution-pure? dist) +inf.0)
   ((and (distribution-statistics dist)
         (statistics-size (distribution-statistics dist))))
   ((distribution-population dist)
    (seq-length (distribution-population dist)))
   ((distribution-summary dist)
    (reservoir-count (summary-sample (distribution-summary dist))))
   (else #f)))

;;> \subsection{Central Tendencies}

;;> Central tendencies measure a central or typical value of a
;;> distribution.  The arithmetic \scheme{mean},
;;> \scheme{geometric-mean} and \scheme{harmonic-mean} are the three
;;> classical Pythagorean means.  The \scheme{mean}, \scheme{median}
;;> and \scheme{mode} can be unified as:
;;>
;;> \schemeblock{
;;>  (mode xi)   => (argmin s (sum (expt (abs (- xi s)) 0)))
;;>  (median xi) => (argmin s (sum (expt (abs (- xi s)) 1)))
;;>  (mean xi)   => (argmin s (sum (expt (abs (- xi s)) 2)))}

;;> \procedure{(mean dist [getter])}
;;> Returns the arithmetic-mean, aka the first moment, what people
;;> typically think of as the average.  As with other procedures in
;;> this library, the computation should be numerically stable.
;;>
;;> \example{(mean '(1 2 3))}
;;> \example{(mean '((a . 1) (b . 2) (c . 3)) cdr)}
;;> \example{(mean (make-list 1000000 #i1/3))}
;;> \example{(mean (normal-distribution 2.4 7))}
(define mean
  (dist-dispatch statistics-mean statistics-mean-set! summary-mean seq-mean))

;;> \procedure{(geometric-mean dist [getter])}
;;> Returns the geometric mean, which is the central tendency based on
;;> the product of the values, rather than the sum as in the
;;> arithmetic mean.  In geometric terms, the geometric mean of two
;;> numbers, a and b, is the length of one side of a square whose area
;;> is equal to that of a rectangle with sides a and b.
;;>
;;> The \scheme{geometric-mean} is often used for numbers whose values
;;> are exponential in nature, such as population growth or financial
;;> investments.
(define geometric-mean
  (dist-dispatch #f #f #f seq-geometric-mean))

;;> \procedure{(harmonic-mean dist [getter])}
;;> Returns the harmonic mean, aka the F-1 score, of the distribution.
;;> This is the reciprocal of the arithmetic \scheme{mean} of the
;;> reciprocals of the values.  The \scheme{harmonic-mean} is often
;;> used in machine learning as an aggregated performance score over
;;> other metrics (such as precision and recall), which penalizes a
;;> single low score harder than the arithmetic or geometric means
;;> would.
(define harmonic-mean
  (dist-dispatch #f #f #f seq-harmonic-mean))

;;> \procedure{(quadratic-mean dist [getter])}
;;> Returns the quadratic mean of the distribution, aka the
;;> root-mean-squared (RMS), which is the square root of the mean
;;> square (the generalized mean with exponent 2).  The RMS of a
;;> deviation or error (RMSE) is frequently used in machine learning
;;> to evaluate the fitness of a model.
(define quadratic-mean
  (dist-dispatch #f #f #f seq-quadratic-mean))

;;> \procedure{(median dist [getter])}
;;> Returns the median of a distribution, the 50th percentile or
;;> "middle" value if the elements were sorted.
(define median
  (dist-dispatch statistics-median statistics-median-set! #f seq-median))

;;> \procedure{(mode dist [getter])}
;;> Returns the mode, or most frequently occuring element of the
;;> distribution.
(define mode
  (dist-dispatch statistics-mode statistics-mode-set! #f seq-mode))

;;> \subsection{Central Moments}

;;> \procedure{(variance dist [getter])}
;;> Returns the variance, aka the second central moment, of a
;;> distribution, defined as the expectation of the squared deviation
;;> from the mean.  Variance is used extensively in descriptive and
;;> inferential statistics.
(define variance
  (let ((sample-var (dist-dispatch statistics-variance statistics-variance-set! summary-variance seq-variance))
        (pop-var (dist-dispatch statistics-variance statistics-variance-set! summary-variance seq-pop-variance)))
    (lambda (dist . o)
      (if (and (distribution? dist) (distribution-population? dist))
          (apply pop-var dist o)
          (apply sample-var dist o)))))

;;> \procedure{(standard-deviation dist [getter])}
;;> Returns the standard deviation, the square root of the
;;> \scheme{variance}.  Unlike \scheme{variance} this is expressed in
;;> the same unit as the data, and can thus be easier to reason about.
;;> In particular, the 68-95-99.7 rule states that 68% of the
;;> population is within 1 stdev of the mean, 95% within 2 stdevs, and
;;> 99.7% within 3 stdevs.
;;>
;;> A simple form of outlier removal is to remove data points more
;;> than 3 (or 4) standard deviations from the mean.
(define (standard-deviation dist . o)
  (sqrt (apply variance dist o)))

;;> \procedure{(stdev dist [getter])}
;;> An alias for \scheme{standard-deviation}.
(define stdev standard-deviation)

;;> Returns the weighted pooled variance of the distributions in the
;;> list \var{dists}.  If any of the distributions are pure (infinite
;;> size and therefore weight) a simpel mean of their variances is
;;> taken.
(define (pooled-variance dists)
  (let ((pure-dists (filter distribution-pure? dists)))
    (if (pair? pure-dists)
        (mean (map variance pure-dists))
        (/ (map-sum (lambda (dist) (* (- (size dist) 1) (variance dist)))
                    dists)
           (- (map-sum size dists) (length dists))))))

;;> Returns the weighted pooled standard deviation of the
;;> distributions \var{dists}, i.e. the sqrt of the pooled-variance.
(define (pooled-standard-deviation dists)
  (sqrt (pooled-variance dists)))

;;> \procedure{(coefficient-of-variation dist [getter])}
;;> Returns the coefficient of variation, aka the relative standard
;;> deviation (RSD) of \var{dist}, the ratio of the
;;> \scheme{standard-deviation} to the \scheme{mean}.
(define (coefficient-of-variation dist . o)
  (/ (apply stdev dist o) (apply mean dist o)))

;;> \procedure{(covariance dist1 dist2 [getter1 [getter2]])}
;;> Returns the covariance, a measure of the joint variability, of the
;;> two (non-pure) distributions \var{dist1} and \var{dist2}.  The
;;> sign of the covariance shows the tendency in the linear
;;> relationship between the variables, though the magnitude is hard
;;> to interpret.  For a better understanding of the strength of the
;;> relation, use the \scheme{pearson-correlation}.
(define (covariance dist1 dist2 . o)
  (assert (not (distribution-pure? dist1))
          (not (distribution-pure? dist2)))
  ;; TODO: non-uniform probabilities
  (let ((mu1 (apply mean dist1 o))
        (mu2 (apply mean dist2 (if (pair? o) (cdr o) o))))
    (/ (seq-fold (lambda (sum elt1 elt2)
                   (+ sum (* (- elt1 mu1) (- elt2 mu2))))
                 0
                 (apply distribution-values dist1 o)
                 (apply distribution-values dist2 (if (pair? o) (cdr o) o)))
       (if (and (distribution? dist1) (distribution-population? dist1))
           (size dist1)
           (- (size dist1) 1)))))

;;> \procedure{(pearson-correlation dist1 dist2 [getter1 [getter2]])}
;;> Returns the Pearson correlation coefficient (PCC), aka Pearson's
;;> r, the Pearson product-moment correlation coefficient (PPMCC), or
;;> the bivariate correlation.  This is a value between [-1, 1]
;;> showing the correlation between the two distributions \var{dist1}
;;> and \var{dist2}, with positive values indicating a positive linear
;;> correlation, negative values indicating a negative linear
;;> correlation, and values close to zero indicating no correlation.
(define (pearson-correlation dist1 dist2 . o)
  (/ (apply covariance dist1 dist2 o)
     (apply standard-deviation dist1 o)
     (apply standard-deviation dist2 (if (pair? o) (cdr o) '()))))

;;> \procedure{(spearman-rank-correlation dist1 dist2 [getter1 [getter2]])}
;;> Returns the Spearman's rank correlation coefficient, aka
;;> Spearman's ρ.  This is the \scheme{pearson-correlation} between
;;> the rank variables of \var{dist1} and \var{dist2}, indicating how
;;> well their relationship can be described by a monotonic function.
(define (spearman-rank-correlation dist1 dist2)
  (assert (not (distribution-pure? dist1))
          (not (distribution-pure? dist2)))
  (let ((r1 (map-rank dist1 <))
        (r2 (map-rank dist2 <)))
    (if (and (every exact-integer? r1)
             (every exact-integer? r2))
        ;; shortcut when there are no ties
        (let ((len (size dist1)))
          (- 1 (/ (* 6 (square-sum (seq-map - r1 r2)))
                  (* len (- (square len) 1)))))
        (pearson-correlation r1 r2))))

;;> \procedure{(skewness dist [getter])}
;;> Returns the skewness, aka the third standardized moment, of
;;> \var{dist}, which indicates the lopsidedness of the distribution.
;;> For a unimodal distribution, negative skew indicates the tail is
;;> to the left of the mode, and positive skew indicates it is to the
;;> right.  Zero skew indicates the tails balance out, though may not
;;> be symmetric.
(define skewness
  (dist-dispatch statistics-skew statistics-skew-set! #f seq-skew))

;;> \procedure{(kurtosis dist [getter])}
;;> Returns the kurtosis, aka the fourth standardizes moment, of
;;> \var{dist}, indicating the heaviness of the tail.  A
;;> \scheme{kurtosis} less than 3 is called "platykurtic", suggesting
;;> the distribution is flat or has fewer extremes than a normal
;;> distribution, whereas a \scheme{kurtosis} greater than 3 is called
;;> "leptokurtic", and has more outliers than a normal distribution.
(define kurtosis
  (dist-dispatch statistics-kurtosis statistics-kurtosis-set! #f seq-kurtosis))

;;> \procedure{(excess-kurtosis dist [getter])}
;;> Returns the excess kurtosis, aka Pearson's kurtosis, of
;;> \var{dist}.  The \scheme{kurtosis} of any normal distribution is
;;> 3, so this is defined as the kurtosis minus 3.
(define (excess-kurtosis dist . o)
  (- (apply kurtosis dist o) 3))

;; TODO: coskewness and cokurtosis

;;> \subsection{Rank Statistics}

;;> \procedure{(maximum dist [getter])}
;;> Returns the maximum value of \var{dist}.
(define maximum
  (dist-dispatch statistics-maximum statistics-maximum-set! #f seq-maximum))

;;> \procedure{(minimum dist [getter])}
;;> Returns the minimum value of \var{dist}.
(define minimum
  (dist-dispatch statistics-minimum statistics-minimum-set! #f seq-minimum))

;;> \procedure{(percentile dist n [getter])}
;;> Returns the \var{n}th percentile of \var{dist}.  If \var{dist} is
;;> non-pure and the rank doesn't fall on an exact position, uses
;;> linear interpolation between adjacent ranks, which produces better
;;> results for small samples.
(define (percentile dist n . o)
  ;; TODO: online percentile, parallel percentile
  (define (vector-min vec start)
    (let lp ((i (+ start 1))
             (lo (vector-ref vec start)))
      (if (= i (vector-length vec))
          lo
          (lp (+ i 1) (min lo (vector-ref vec i))))))
  (cond
   ((distribution-pure? dist)
    ((inverse-transform-cdf (distribution-cdf dist)
                            (distribution-statistics dist))
     (/ n 100.)))
   ((and (distribution? dist)
         (distribution-summary dist)
         (histogram? (summary-density (distribution-summary dist))))
    (histogram-quantile (summary-density (distribution-summary dist))
                        (/ n 100.)))
   (else
    (let* ((seq (apply distribution-values dist o))
           (less (if (pair? o) (car o) <))
           (mean (if (and (pair? o) (pair? (cdr o)))
                     (cadr o)
                     (lambda (a b) (/ (+ a b) 2))))
           (vec (if (vector? seq) (vector-copy seq) (list->vector seq)))
           (len (vector-length vec))
           (i (* (- len 1) (/ n 100))))
      (cond
       ((zero? len) #f)
       (else
        (let ((i_ (exact (floor i))))
          ;; TODO: cache the sorted values
          (vector-separate! less vec i_)
          (if (or (integer? i) (>= (+ i 1) len))
              (vector-ref vec i_)
              (let ((xi (vector-ref vec i_))
                    (xi+1 (vector-min vec (+ i_ 1))))
                (+ (* (- (+ i_ 1) i) xi)
                   (* (- i i_) xi+1)))))))))))

;;> \procedure{(rank ls [getter])}
;;> Ranks a sorted list (x1 x2 ... xn) returning
;;>   \schemeblock{((rank1 . x1) (rank2 . x2) ... xn)}
;;> where the ranks are 1..n if there all xi are unique,
;;> otherwise the average rank among equal elements.
(define (rank ls . o)
  (if (null? ls)
      '()
      (let ((less (if (pair? o) (car o) <))
            (get-key (if (and (pair? o) (pair? (cdr o))) (cadr o) values)))
        (let lp ((ls (cdr ls))
                 (cur (list (cons 1 (car ls))))
                 (key (get-key (car ls)))
                 (i 2)
                 (res '()))
          (define (collect)
            (let ((r (mean cur car)))
              (append-reverse (map (lambda (x) (cons r (cdr x))) cur) res)))
          (if (null? ls)
              (reverse (collect))
              (let ((key2 (get-key (car ls))))
                (cond
                 ((less key key2)
                  (lp (cdr ls)
                      (list (cons i (car ls)))
                      key2
                      (+ i 1)
                      (collect)))
                 ((less key2 key)
                  (error "rank: lists must be sorted"))
                 (else  ;; equal
                  (lp (cdr ls)
                      (cons (cons i (car ls)) cur)
                      key
                      (+ i 1)
                      res)))))))))

;;> Returns a new list with the elements replaced by their rank.
(define (map-rank dist . o)
  (let* ((less (if (pair? o) (car o) <))
         (get-key (if (and (pair? o) (pair? (cdr o))) (cadr o) values))
         (get-key2 (lambda (x) (get-key (car x))))
         (seq (distribution-values dist))
         (len (seq-length seq))
         (indexed (map cons
                       (if (vector? seq) (vector->list seq) seq)
                       (iota len)))
         (ordered (sort indexed less get-key2))
         (ranked (rank ordered less get-key2))
         (res (make-vector len)))
    (do ((ls ranked (cdr ls)))
        ((null? ls)
         (if (vector? seq) res (vector->list res)))
      (vector-set! res (cdr (cdar ls)) (caar ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{Sampling}

;;> Returns a random flip of a weighted coin, i.e. runs a Bernoulli
;;> trial, returning \scheme{#t} with probability \var{p} (default
;;> 0.5) and \scheme{#f} otherwise.
(define (flip? . o)
  (let ((p (if (pair? o) (car o) 0.5)))
    (< (random-real) p)))

;;> Returns a random value from distribution \var{dist}.
(define (random-pick dist)
  (seq-ref (random-sample 1 dist) 0))

;;> Returns a sequence of \var{n} random value from distribution
;;> \var{dist}.  If \var{dist} is a list or distribution based on one
;;> the result is a list, otherwise it's a vector.
(define (random-sample n dist . o)
  (if (distribution-pure? dist)
      (let ((res (make-vector n)))
        (do ((i 0 (+ i 1)))
            ((>= i n) res)
          (vector-set! res i ((distribution-random dist)))))
      (let* ((res (make-vector n))
             (seq (distribution-values dist))
             (len (seq-length seq)))
        (let lp ((i 0) (ls dist) (m 0))
          (cond
           ((>= m n)
            (if (vector? seq) res (vector->list res)))
           ((>= (* (- len i) (random-real)) (- n m))
            (lp (+ i 1) (if (pair? ls) (cdr ls) ls) m))
           (else
            (vector-set! res m (if (pair? ls) (car ls) (seq-ref seq i)))
            (lp (+ i 1) (if (pair? ls) (cdr ls) ls) (+ m 1))))))))

;; ordered multi-sets as a minimal wrapper around SRFI 146 mappings

(define-record-type Ordered-Set
  (%make-ordered-set map)
  ordered-set?
  (map oset-map oset-map-set!))

(define real-comparator
  (make-comparator real? = < hash))

(define (make-ordered-set . o)
  (%make-ordered-set
   (mapping/ordered (if (pair? o) (car o) real-comparator))))
(define (ordered-set-add! os elt)
  (let ((map (mapping-update!/default
              (oset-map os) elt (lambda (ls) (cons elt ls)) '())))
    (oset-map-set! os map)))
(define (ordered-set-pop-first! os)
  (let* ((map (oset-map os))
         (key (mapping-min-key map))
         (val (mapping-min-value map)))
    (if (not (pair? val))
        (error "inconsistent state" key val))
    (oset-map-set!
     os
     (if (pair? (cdr val))
         (mapping-set! map key (cdr val))
         (call-with-values (lambda () (mapping-pop! map))
           (lambda (new-map key val) new-map))))
    (car val)))
(define (ordered-set-pop-last! os)
  (let* ((map (oset-map os))
         (key (mapping-max-key map))
         (val (mapping-max-value map)))
    (if (not (pair? val))
        (error "inconsistent state" key val))
    (oset-map-set!
     os
     (if (pair? (cdr val))
         (mapping-set! map key (cdr val))
         (mapping-delete! map key)))
    (car val)))

(define-record-type Alias
  (make-alias element prob alias)
  alias?
  (element alias-element)
  (prob alias-prob alias-prob-set!)
  (alias alias-alias alias-alias-set!))

(define-syntax alias-prob-dec!
  (syntax-rules ()
    ((alias-prob-dec! alias x)
     (let ((a alias))
       (alias-prob-set! a (- (alias-prob a) x))))))

(define alias-comparator
  (let ((alias< (lambda (a b) (< (alias-prob a) (alias-prob b)))))
    (make-comparator alias? equal? alias< hash)))

(define (make-multi-sampler seq get-norm-weight)
  ;; Exercise 7 in TAoCP Vol 2. Sect. 3.4.1
  (let* ((n (seq-length seq))
         (res (make-vector n))
         (q (make-ordered-set alias-comparator)))
    ;; Enqueue the elements, scaling weights by n since hitting any
    ;; element in the lookup table has probability 1/n.
    (seq-for-each
     (lambda (elt)
       (ordered-set-add! q (make-alias elt (* n (get-norm-weight elt)) elt)))
     seq)
    (do ((i 0 (+ i 1)))
        ((>= i (- n 1)))
      ;; On each pass, make the largest remaining prob element the
      ;; alias for the smallest, and update the prob for largest.
      ;; Invariant: the sum of probs of unprocessed elements is i/n
      (let* ((smallest (ordered-set-pop-first! q))
             (largest (ordered-set-pop-last! q)))
        (if (not (and smallest largest))
            (error "inconsistency, couldn't build alias table" seq))
        (vector-set! res i smallest)
        (alias-alias-set! smallest (alias-alias largest))
        (alias-prob-dec! largest (- 1 (alias-prob smallest)))
        (ordered-set-add! q largest)))
    ;; By the above invariant the last should have probability ~1.
    (let ((last (ordered-set-pop-first! q)))
      (when (> (abs (- 1 (alias-prob last))) 1e-6)
        (error "couldn't build alias table - are the weights normalized?"
               seq (alias-prob last)))
      (alias-prob-set! last 1)
      (vector-set! res (- n 1) last)
      res)))

;;> Like \scheme{random-sample}, but returns a sample with replacement.
(define (random-multi-sample n x . o)
  ;; Alias method by A.J. Walker ("New fast method for generating
  ;; discrete random numbers with arbitrary frequency distributions,"
  ;; Electronics Letters 10, 1974).
  ;;
  ;; Setup a table with one entry for each element, having a
  ;; probability and an alias.  Choose an entry at random, then
  ;; compare another random value with the corresponding probability -
  ;; if smaller, use the entry, otherwise use the alias.  The table
  ;; should be generated such that the sum of each entries'
  ;; probabilities (including indirect probabilities via aliasing)
  ;; should be equal to the original probability.
  (cond
   ((distribution-pure? x)
    (let ((res (make-vector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) res)
        (vector-set! res i ((distribution-random x))))))
   (else
    (let* ((seq (distribution-values x))
           (get-weight (cond ((null? o)
                              (lambda (x) 1))
                             ((hash-table? (car o))
                              (lambda (x) (hash-table-ref/default (car o) x 1)))
                             (else (car o))))
           (total-weight (map-sum get-weight seq)))
      (when (<= total-weight 0)
        (error "total weight must be positive"))
      (let ((table
             (make-multi-sampler seq (lambda (x) (/ (get-weight x) total-weight))))
            (res (make-vector n)))
        (do ((i 0 (+ i 1)))
            ((= i n) (if (vector? seq) res (vector->list res)))
          (let* ((alias (random-pick table))
                 (elt (if (flip? (alias-prob alias))
                          (alias-element alias)
                          (alias-alias alias))))
            (vector-set! res i elt))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample size estimates

;;> \procedure{(min-sample-size num-total [conf err p])}
;;>
;;> Compute the minimum sample size for a population num-total, with
;;> the given confidence, error bound requirements, and population
;;> proportion.
;;>
;;> \itemlist[
;;> \item{num-total: total population size
;;>            (+inf.0 uses the normal approximation to binomial dist, otherwise
;;>             we use the normal approximation to the hypergeometric dist)}
;;> \item{conf: desired level of confidence, default 1.96 (95%)}
;;> \item{err: desired error bounds, default 3%}
;;> \item{p: population proportion, default 0.5}
;;> ]
;;>
;;> For example, assume you want a 99% confidence (conf: 2.75) that the
;;> result is within 1%, then:
;;>
;;>   \example{(min-sample-size +inf.0 2.75 0.01)}
;;>
;;> however if you know the total population the required size may be
;;> smaller (but never larger):
;;>
;;>   \example{(min-sample-size 73015 2.75 0.01)}
(define (min-sample-size num-total . o)
  (let* ((conf (if (pair? o) (car o) 1.96))
         (o (if (pair? o) (cdr o) o))
         (err (if (pair? o) (car o) 0.03))
         (o (if (pair? o) (cdr o) o))
         (p (if (pair? o) (car o) 0.5)) (q (- 1.0 p)))
    (if (>= num-total +inf.0)
        (/ (* conf conf p q)
           (* err err))
        (/ (* num-total conf conf p q)
           (+ (* err err (- num-total 1)) (* conf conf p q))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{Inferential Statistics}

;;> \subsection{Hypothesis Testing}

;; requires knowing the mean and stdev of the population
(define (standard-score x mu . o)
  (let ((sigma (if (pair? o) (car o) 1)))
    (/ (- x mu) sigma)))

;;> Performs a Z-test, a test that a \var{sample} can be approximated
;;> by a normal distribution.  Returns the probability that
;;> \var{sample} is consistent with \var{dist}, default one-tailed.
;;> This is more convenient than the T-test if the population size is
;;> large or the variance is known.
;;>
;;> \var{tails} must be 1 (the default) for a one-tailed test, or 2
;;> for a two-tailed test.  If you are testing whether a mean is
;;> greater (or less) than the population, you want a one-tailed test,
;;> but if you are just testing whether it is different (in either
;;> direction) you want a two-tailed test.
;;>
;;> Example: We claim it takes Schemers longer to fix reported bugs
;;> because they get distracted finding the perfect solution to the
;;> problem.  Querying the
;;> \hyperlink[https://developer.github.com/v3/issues/]{Github API}
;;> indicates that the overall mean time to close an issue is 7.8 days
;;> with a variance of 23.1 (made up numbers).  Restricting this to
;;> just the 5,197 Scheme repositories yields a mean time to close of
;;> 8.3 days.  Is our claim valid?  Assuming the distribution for the
;;> time to fix bugs is normal, we can run a \scheme{z-test}:
;;>
;;> \example{
;;> (z-test (summary-distribution 'mean: 8.3 'size: 5197)
;;>         (normal-distribution 7.8 23.1))}
;;>
;;> This is greater than 0.05, so at a 95% confidence interval we
;;> would reject our hypothesis - Schemers are just as slow as
;;> everbody else.
(define (z-test sample dist . o)
  (let ((tails (if (pair? o) (car o) 1))
        (stat (z-statistic sample dist)))
    (assert (and (exact-integer? tails) (<= 1 tails 2)))
    (* tails (normal-cdf (- (abs stat)) 0 1))))

;;> Returns true iff the \scheme{z-test} for \var{sample} and
;;> \var{dist} meets the given \var{alpha} level, default 5%.
;;>
;;> \example{
;;> (z-test? (summary-distribution 'mean: 8.3 'size: 5197)
;;>          (normal-distribution 7.8 23.1))}
(define (z-test? sample dist . o)
  (let ((alpha (if (pair? o) (car o) 0.05))
        (tails (if (and (pair? o) (pair? (cdr o))) (cadr o) 1)))
    (<= (z-test sample dist tails) alpha)))

;;> Returns the underlying Z statistic showing how well \var{dist}
;;> approximates \var{sample}.
;;>
;;> \example{
;;> (z-statistic (summary-distribution 'mean: 8.3 'size: 5197)
;;>              (normal-distribution 7.8 23.1))}
(define (z-statistic sample dist)
  (if (and (distribution-pure? dist)
           (not (eq? 'normal (distribution-name dist))))
      (error "z-statistic expected a normal distribution" dist))
  (let ((sigma (pooled-standard-deviation (list sample dist))))
    (standard-score (mean sample) (mean dist) sigma)))

;;> AKA the Student's t-test, after the pen name "Student" used by
;;> William Sealy Gosset.  Returns the probability that the two
;;> distributions \var{dist1} and \var{dist2} have the same mean,
;;> under the assumption the underlying distributions are normal.
;;> \var{tails} defaults to 1 and has the same semantics as in the
;;> \scheme{z-test}.
;;>
;;> Example: You made a micro-optimization to your code but the
;;> benchmark results are inconsistent, sometimes even being slower
;;> than before.  The average of the times is faster, but you're not
;;> sure if this is a real improvement or just noise.  Assuming the
;;> noise is normally distributed, we can run a \scheme{t-test} on the
;;> times:
;;>
;;> \example{
;;> (let ((before '(30.02 29.99 30.11 29.97 30.01 29.99))
;;>       (after  '(29.89 29.93 29.72 29.98 30.02 29.98)))
;;>   (t-test after before))}
;;>
;;> This is less than a 0.05 alpha, so we can assume the improvement
;;> is significant, if small (0.3%):
;;>
;;> \example{
;;> (gain (mean '(30.02 29.99 30.11 29.97 30.01 29.99))
;;>       (mean '(29.89 29.93 29.72 29.98 30.02 29.98)))}
(define (t-test dist1 dist2 . o)
  (let ((tails (if (pair? o) (car o) 1))
        (stat (t-statistic dist1 dist2))
        (df (t-df dist1 dist2)))
    (assert (and (exact-integer? tails) (<= 1 tails 2)))
    ;; An alternate formula, valid only if t^2<df, uses the
    ;; hypergeometric function.
    (let ((res (beta (* 0.5 df) 0.5 (/ df (+ df (square stat))))))
      (if (= 2 tails)
          res
          (/ res 2)))))

;;> Returns true iff the \scheme{t-test} for \var{dist1} and
;;> \var{dist2} meets the given \var{alpha} level, default 5%.
;;>
;;> Example: Given a random sample of Schemer's IQs, can we assume
;;> they are smarter than the overall population?
;;>
;;> \example{(t-test? '(120 115 95 135) (normal-distribution 100 225))}
(define (t-test? dist1 dist2 . o)
  (let ((alpha (if (pair? o) (car o) 0.05))
        (tails (if (and (pair? o) (pair? (cdr o))) (cadr o) 1)))
    (< (t-test dist1 dist2 tails) alpha)))

;;> Returns the underlying t statistic showing how well \var{dist1}
;;> matches \var{dist2}.
;;>
;;> \example{(t-test? '(120 115 95 135) (normal-distribution 100 225))}
(define (t-statistic dist1 dist2)
  (cond
   ((not (distribution? dist1))
    (t-statistic (distribution dist1) dist2))
   ((not (distribution? dist2))
    (t-statistic dist1 (distribution dist2)))
   ((distribution-pure? dist1)
    (if (distribution-pure? dist2)
        (error "t-test comparing two pure distributions" dist1 dist2)
        (t-statistic dist2 dist1)))
   ((distribution-pure? dist2)
    (/ (- (mean dist1) (mean dist2))
       (/ (standard-deviation dist1)
          (sqrt (size dist1)))))
   (else
    (let* ((m1 (mean dist1))
           (m2 (mean dist2))
           (s1 (standard-deviation dist1))
           (s2 (standard-deviation dist2))
           (n1 (size dist1))
           (n2 (size dist2)))
      (if (< 1/2 (/ s1 s2) 2)
          (/ (- m1 m2)
             (* (harmonic-mean (list n1 n2))
                (pooled-standard-deviation (list dist1 dist2))))
          ;; Welch's t-test (unequal variance)
          (/ (- m1 m2)
             (sqrt (+ (/ (square s1) n1)
                      (/ (square s2) n2)))))))))

;;> Returns the degrees of freedom for the \scheme{t-test} comparing
;;> \var{dist1} and \var{dist2}.
;;>
;;> \example{(t-df '(120 115 95 135) (normal-distribution 100 225))}
(define (t-df dist1 dist2)
  (cond
   ((not (distribution? dist1))
    (t-df (distribution dist1) dist2))
   ((not (distribution? dist2))
    (t-df dist1 (distribution dist2)))
   ((distribution-pure? dist1)
    (if (distribution-pure? dist2)
        (error "comparing two pure distributions" dist1 dist2)
        (t-df dist2 dist1)))
   ((distribution-pure? dist2)
    (- (size dist1) 1))
   (else
    (let* ((s1 (standard-deviation dist1))
           (s2 (standard-deviation dist2))
           (s1^2 (square s1))
           (s2^2 (square s2))
           (n1 (size dist1))
           (n2 (size dist2)))
      (if (< 1/2 (/ s1 s2) 2)
          (+ n1 n2 -2)
          ;; aka the Welch–Satterthwaite equation
          (/ (square (+ (/ s1^2 n1) (/ s2^2 n2)))
             (+ (/ (square (/ s1^2 n1)) (- n1 1))
                (/ (square (/ s2^2 n2)) (- n2 1)))))))))

;; TODO: paired t-test

;; Paired tests test the distribution of differences in pairs -
;; typically against the null hypothesis that the mean is 0.
;; The distributions should have empirical samples which are
;; assumed to be aligned in order.  For convenience, a pure
;; discrete distribution can be used as the expected dist.
;; In this case, the sum of the observed distribution is taken
;; as the original sample size, and the expected values are
;; their probabilities scaled by this amount.

(define (paired-dispatch proc)
  (lambda (observed expected)
    (assert (not (distribution-pure? observed)))
    (let* ((ob (distribution-values observed))
           (ex (if (distribution-pure? expected)
                   (if (distribution-discrete? expected)
                       (let ((n (sum ob))
                             (pmf (distribution-pdf expected)))
                         (seq-map (lambda (x) (* n (pmf x)))
                                  (iota (seq-length ob) (minimum expected))))
                       (error "paired test distributions must be discrete"
                              expected))
                   (distribution-values expected))))
      (proc ob ex))))

;;> Performs Pearson's chi-squared test (χ²) to determine the
;;> goodness of fit between the \var{observed} and \var{expected}
;;> distributions.  Returns the p-value.
;;>
;;> Example: We want to know if our new PRNG is generating evenly
;;> distributed die rolls.  We can test this by comparing a histogram
;;> of the values generated for 1 through 6, against the expected
;;> uniform distribution:
;;>
;;> \example{
;;> (let ((rolls-histogram '#(5 8 9 8 10 20)))
;;>   (chi^2-test rolls-histogram (discrete-uniform-distribution 1 6)))}
;;>
;;> Since this is less than the 0.05 alpha we reject the null
;;> hypothesis that the PRNG generates a uniform distribution, i.e. it
;;> must be biased.
(define (chi^2-test observed expected)
  (if (real? expected)
      (let ((stat observed)
            (df expected))
        (- 1
           (lower-incomplete-gamma
            (/ df 2)
            (/ stat 2))))
      (chi^2-test (chi^2-statistic observed expected)
                  (chi^2-df observed))))

;;> Returns \scheme{#t} iff the p-value from \scheme{chi^2-test} is
;;> less than \var{alpha} (default 0.05), i.e. iff the two
;;> distributions differ.
;;>
;;> \example{
;;> (let ((rolls-histogram '#(5 8 9 8 10 20)))
;;>   (chi^2-test? rolls-histogram (discrete-uniform-distribution 1 6)))}
(define (chi^2-test? observed expected . o)
  (let ((alpha (if (pair? o) (car o) 0.05)))
    (< (chi^2-test observed expected) alpha)))

;;> \procedure{(chi^2-statistic observed expected)}
;;> Returns the underlying chi^2 statistic for \scheme{chi^2-test}.
;;>
;;> \example{
;;> (let ((rolls-histogram '#(5 8 9 8 10 20)))
;;>   (chi^2-statistic rolls-histogram (discrete-uniform-distribution 1 6)))}
(define chi^2-statistic
  (paired-dispatch
   (lambda (observed expected)
     (map-sum (lambda (o e) (if (zero? e) 0 (/ (square (- o e)) e)))
              observed
              expected))))

;;> Returns the degrees of freedom for a chi^2 goodness of fit test
;;> (as well as g-test).
;;>
;;> \example{(chi^2-df '#(5 8 9 8 10 20))}
(define (chi^2-df observed . o)
  (let ((params (if (pair? o) (car o) 1)))
    (- (size observed) params)))

;; TODO: homogeneity and independence tests
;; (i.e. chi-squared over a contingency table)

;;> Performs a G-test for goodness of fit between the \var{observed}
;;> and \var{expected} distributions, which performs better than the
;;> chi^2 test in many cases.  Returns the p-value.
;;>
;;> Example: Your university claimed equal enrollment for men and
;;> women in Computer Science, but when you show up the first day to
;;> COMP 101 there are only 42 girls out of a class of 100.  Is the
;;> 42:58 ratio significantly different from the expected 50:50?
;;>
;;> \example{(g-test '(42 58) '(50 50))}
;;>
;;> Since this is greater than a 0.05 alpha we can't assume it is
;;> significant, i.e. this split is within the range of the
;;> universities' claim.
(define (g-test observed expected)
  (if (real? observed)
      (let ((stat observed)
            (df expected))
        (chi^2-test stat df))
      (g-test (g-statistic observed expected)
              (chi^2-df observed))))

;;> Returns \scheme{#t} iff the p-value from \scheme{g-test} is less
;;> than \var{alpha} (default 0.05) i.e. iff the two distributions
;;> differ.
;;>
;;> \example{(g-test? '(42 58) '(50 50))}
(define (g-test? observed expected . o)
  (let ((alpha (if (pair? o) (car o) 0.05)))
    (< (g-test observed expected) alpha)))

;;> \procedure{(g-statistic observed expected)}
;;> Returns the underlying statistic the for \scheme{g-test}.
;;>
;;> \example{(g-statistic '(42 58) '(50 50))}
(define g-statistic
  (paired-dispatch
   (lambda (observed expected)
     (* 2 (map-sum (lambda (o e) (* o (log (/ o e))))
                   observed
                   expected)))))

;;> Performs a binomial test that the \var{observed} distribution
;;> matches the \var{expected}.  This is more accurate than the
;;> \scheme{chi^2-test} and \scheme{g-test} when there are few values.
;;> Returns the p-value.
;;>
;;> Example: A mobile game you play has an option to spin a virtual
;;> capsule machine as an in-game purchase, with each spin giving you
;;> a new character.  They advertise a 3% chance of getting a rare
;;> character.  You pay for 100 spins and get only 1 rare character.
;;> We can run a \scheme{binomial-test} to determine if we believe
;;> their advertised probability:
;;>
;;> \example{
;;> (binomial-test (bernoulli-trials 1 100) (mean-distribution 0.03))}
;;>
;;> The p-value is less than 0.05 so you should probably demand your
;;> money back.
(define (binomial-test observed expected . o)
  (let* ((tails (if (pair? o) (car o) 1))
         (n (size observed))
         (successes (* n (mean observed)))
         (p (mean expected))
         (cdf (distribution-cdf (binomial-distribution n p))))
    (assert (and (exact-integer? tails) (<= 1 tails 2)))
    (cond
     ((= tails 2)
      (let* ((diff (- p (mean observed)))
             (alt (exact (floor (+ 1 (* (+ p diff) n))))))
        (+ (- 1 (cdf successes))
           (cdf alt))))
     ((< (/ successes n) p)
      (cdf successes))
     (else
      (- 1 (cdf successes))))))

;;> Returns \scheme{#t} iff the p-value from \scheme{binomial-test} is
;;> less than \var{alpha}, default 0.05.
;;>
;;> \example{
;;> (binomial-test? (bernoulli-trials 1 100) (mean-distribution 0.03))}
(define (binomial-test? observed expected . o)
  (let ((tails (if (pair? o) (car o) 1))
        (alpha (if (and (pair? o) (pair? (cdr o))) (cadr o) 0.05)))
    (< (binomial-test observed expected tails) alpha)))

(define (kolmogorov-cdf n d)
  ;; Adapted from Evaluating Kolmogorov's Distribution, George
  ;; Marsaglia, Wai Wan Tsang, Jingbo Wang in the references.
  (let* ((k (exact (truncate (+ 1 (* n d)))))
         (m (- (* 2 k) 1))
         (h (- k (* n d)))
         (s (* d d n))
         (H (make-vector (* m m) 0)))
    (cond
     ((or (> s 7.24) (and (> s 3.76) (> n 99)))
      (- 1 (* 2 (exp (* s (- (+ 2.000071
                                (/ .331 (sqrt n))
                                (/ 1.409 n))))))))
     (else
      (letrec-syntax
          ((matrix-ref
            (syntax-rules ()
              ((matrix-ref A i j) (vector-ref A (+ j (* i m))))))
           (matrix-set!
            (syntax-rules ()
              ((matrix-set! A i j v) (vector-set! A (+ j (* i m)) v))))
           (matrix-inc!
            (syntax-rules ()
              ((matrix-inc! A i j v)
               (let* ((A A)
                      (x (+ j (* i m)))
                      (old (vector-ref A x)))
                 (vector-set! A x (+ old v))))))
           (matrix-dec!
            (syntax-rules ()
              ((matrix-dec! A i j v) (matrix-inc! A i j (- v)))))
           (matrix-div!
            (syntax-rules ()
              ((matrix-div! A i j v)
               (let* ((A A)
                      (x (+ j (* i m)))
                      (old (vector-ref A x)))
                 (vector-set! A x (/ old v)))))))
        (define (matrix-id)
          (let ((res (make-vector (* m m) 0)))
            (do ((i 0 (+ 1 i)))
                ((= i m) res)
              (matrix-set! res i i 1))))
        (define (matrix-mul a b)
          (let ((res (make-vector (* m m))))
            (do ((i 0 (+ i 1)))
                ((= i m) res)
              (do ((j 0 (+ j 1)))
                  ((= j m))
                (do ((k 0 (+ k 1))
                     (s 0 (+ s (* (matrix-ref a i k) (matrix-ref b k j)))))
                    ((= k m) (matrix-set! res i j s)))))))
        ;; TODO: the original implementation tracks if the values get
        ;; too large and scales to retain precision
        (define (matrix-expt a e)
          (do ((e e (quotient e 2))
               (a a (matrix-mul a a))
               (res (matrix-id) (if (odd? e) (matrix-mul res a) res)))
              ((zero? e) res)))
        (do ((i 0 (+ i 1)))
            ((= i m))
          (do ((j 0 (+ j 1)))
              ((= j m))
            (matrix-set! H i j (if (< (- i j -1) 0) 0 1))))
        (do ((i 0 (+ i 1)))
            ((= i m))
          (matrix-dec! H i 0 (expt h (+ i 1)))
          (matrix-dec! H (- m 1) i (expt h (- m i))))
        (matrix-dec! H (- m 1) 0 (if (positive? (- (* 2 h) 1))
                                     (expt (- (* 2 h) 1) m)
                                     0))
        (do ((i 0 (+ i 1)))
            ((= i m))
          (do ((j 0 (+ j 1)))
              ((= j m))
            (when (positive? (- i j -1))
              (do ((g 1 (+ g 1)))
                  ((> g (- i j -1)))
                (matrix-div! H i j g)))))
        (let ((Q (matrix-expt H n)))
          (let lp ((i 1)
                   (s (matrix-ref Q (- k 1) (- k 1)))
                   (eQ 0))
            (if (> i n)
                (* s (expt 10.0 eQ))
                (let ((s (* s (/ i n))))
                  (if (< s 1e-140)
                      (lp (+ i 1) (* s 1e140) (- eQ 140))
                      (lp (+ i 1) s eQ)))))))))))

;;> Performs a Kolmogorov Smirnoff test, returning the distance
;;> between \var{dist1} and \var{dist2}.  This is frequently used to
;;> determine if two samples come from the same distribution, without
;;> needing to make assumptions about that distribution (e.g. it need
;;> not be normal).
(define (kolmogorov-smirnoff-test dist1 dist2 . o)
  (let* ((strict? (and (pair? o) (car o)))
         (d (kolmogorov-smirnoff-statistic dist1 dist2))
         (m (size dist1))
         (n (size dist2)))
    (if (< (* m n) 10000)
        ;; exact
        (- 1 (/ (lattice-paths m n m n (integrate-d d m n) strict?)
                (combinations (+ n m) m)))
        ;; approx for large tests
        (- 1 (kolmogorov-smirnoff-sum (* d (sqrt (/ (* m n) (+ m n))))
                                      ks-sum-cauchy-criterion
                                      maximum-partial-sum-count)))))

;;> Returns \scheme{#t} iff the \scheme{kolmogorov-smirnoff-test}
;;> value between \var{dist1} and \var{dist2} is greater than the
;;> \scheme{kolmogorov-smirnoff-critical} value.
(define (kolmogorov-smirnoff-test? dist1 dist2 . o)
  (let* ((strict? (and (pair? o) (car o)))
         (alpha (if (pair? o) (car o) 0.05)))
    (> (kolmogorov-smirnoff-test dist1 dist2 strict?)
       (kolmogorov-smirnoff-critical (size dist1) (size dist2) alpha))))

;;> Returns the underlying statistic for the
;;> \scheme{kolmogorov-smirnoff-test}.
(define (kolmogorov-smirnoff-statistic dist1 dist2)
  (when (or (distribution-pure? dist1) (distribution-pure? dist2))
    (error "single sample kolmogorov-smirnoff not yet supported" dist1 dist2))
  ;; TODO: result is inaccurate in the presence of ties, fix this
  ;; e.g. with random jitter
  (let* ((seq1 (sort (distribution-values dist1) <))
         (seq2 (sort (distribution-values dist2) <))
         (n (seq-length seq1))
         (m (seq-length seq2)))
    (let lp1 ((rank1 0)
              (rank2 0)
              (cur-d 0)
              (sup-d 0))
      (if (or (>= rank1 n) (>= rank2 m))
          (/ sup-d (* n m))
          (let ((z (min (seq-ref seq1 rank1)
                        (seq-ref seq2 rank2))))
            (let lp2 ((rank1 rank1)
                      (rank2 rank2)
                      (cur-d cur-d))
              (cond
               ((and (< rank1 n) (= z (seq-ref seq1 rank1)))
                (lp2 (+ rank1 1) rank2 (+ cur-d m)))
               ((and (< rank2 m) (= z (seq-ref seq2 rank2)))
                (lp2 rank1 (+ rank2 1) (- cur-d n)))
               ((> cur-d sup-d)
                (lp1 rank1 rank2 cur-d cur-d))
               ((> (- cur-d) sup-d)
                (lp1 rank1 rank2 cur-d (- cur-d)))
               (else
                (lp1 rank1 rank2 cur-d sup-d)))))))))

(define (kolmogorov-smirnoff-sum t tolerance max-iters)
  (if (zero? t)
      t
      (let ((x (* -2 t t)))
        (let lp ((i 1)
                 (sign -1)
                 (delta 1)
                 (sum 0.5))
          (cond
           ((< delta tolerance)
            (* 2 sum))
           ((> i max-iters)
            (error "too many iterations of kolmogorov-smirnoff-sum"))
           (else
            (let ((delta (exp (* x i i))))
              (lp (+ i 1)
                  (* -1 sign)
                  delta
                  (+ sum (* sign delta))))))))))

(define ks-sum-cauchy-criterion 1e-20)
(define maximum-partial-sum-count 100000)

(define (integrate-d d m n . o)
  (let* ((strict? (and (pair? o) (car o)))
         (nm (* n m))
         (upper (ceiling (* n m (- d 1e-12))))
         (lower (floor (* n m (+ d 1e-12)))))
    (if (and strict? (= lower upper))
        (+ upper 1)
        upper)))

(define (lattice-paths i j m n cnm . o)
  (let ((strict? (and (pair? o) (car o))))
    (define c
      (if strict?
          (lambda (i j m n cnm)
            (if (<= (abs (- (* i n) (* j m))) cnm) 1 0))
          (lambda (i j m n cnm)
            (if (< (abs (- (* i n) (* j m))) cnm) 1 0))))
    (let ((lag (make-vector n 0)))
      (do ((k 0 (+ k 1)))
          ((= k n))
        (vector-set! lag k (c 0 (+ k 1) m n cnm)))
      (let lp1 ((k 1) (last 0))
        (if (> k i)
            last
            (let lp2 ((l 1)
                      (last (c k 0 m n cnm)))
              (cond
               ((> l j)
                (lp1 (+ k 1) last))
               (else
                (vector-set! lag
                             (- l 1)
                             (* (c k l m n cnm)
                                (+ last (vector-ref lag (- l 1)))))
                (lp2 (+ l 1) (vector-ref lag (- l 1)) )))))))))

;; Returns the critical value for the
;; \scheme{kolmogorov-smirnoff-test?}.
(define (kolmogorov-smirnoff-critical n m . o)
  (let ((alpha (if (pair? o) (car o) 0.05)))
    (sqrt (/  (* -1/2
                 (log (/ alpha 2))
                 (+ 1 (/ n m)))
              n))))

;; TODO: wilcoxon-test returning p-value

;;> Performs a Wilcoxon signed ranked test to determine whether the
;;> mean ranks of \var{dist1} and \var{dist2} (which must be paired,
;;> i.e. the elements in aligned order) differ.  It can be used as an
;;> alternative to the \scheme{t-test} when the distributions can't be
;;> assumed to be normal.  Returns \scheme{#t} iff we fail to reject
;;> the null hypothesis, i.e. we assume the distributions are the
;;> same.
(define (wilcoxon-test? dist1 dist2 . o)
  (let* ((tails (if (pair? o) (car o) 1))
         (alpha (if (and (pair? o) (pair? (cdr o))) (cadr o) .05)))
    (call-with-values (lambda () (wilcoxon-statistic dist1 dist2))
      (lambda (w nr)
        (if (< nr 10)
            (< (abs w) (wilcoxon-critical nr tails alpha))
            (> (/ w (sqrt (* nr (+ nr 1) (+ (* nr 2) 1) 1/6)))
               (wilcoxon-critical nr tails alpha)))))))

;;> Returns the underlying statistic for the \scheme{wilcoxon-test?}.
(define (wilcoxon-statistic dist1 dist2)
  ;; (see Mann-Whitney-Wilcoxon for 2 indep samples)
  ;; (see sign test to remove symmetric assumption)
  ;; (consider modification by Pratt to incorporate zero diffs)
  (assert (not (distribution-pure? dist1)) (not (distribution-pure? dist2)))
  (let* ((seq1 (distribution-values dist1))
         (seq2 (distribution-values dist2))
         (n (seq-length seq1)))
    (unless (= n (seq-length seq2))
      (error "wilcoxon: sequences must have the same length" seq1 seq2))
    (let lp ((i 0) (ls '()))
      (if (< i n)
          (let* ((x (seq-ref seq1 i))
                 (y (seq-ref seq2 i))
                 (diff (- x y))
                 (sign (if (< diff 0) -1 1)))
            (if (zero? diff)
                (lp (+ i 1) ls)
                (lp (+ i 1) (cons (cons (abs diff) sign) ls))))
          (let ((nr (length ls))
                (ranked (rank (sort ls < car) < car)))
            ;; older T-statistic (need to adjust critical value for this)
            ;; (let ((pos (filter (lambda (x) (positive? (cddr x))) ranked))
            ;;       (neg (filter (lambda (x) (negative? (cddr x))) ranked)))
            ;;   (min (map-sum car pos) (map-sum car neg)))
            (values (map-sum (lambda (r) (* (car r) (cddr r))) ranked)
                    nr))))))

;;> Returns the critical value for comparison against the statistic in
;;> the \scheme{wilcoxon-test?}.
(define (wilcoxon-critical nr tails alpha)
  (let ((alpha (if (= tails 1) (* alpha 2) alpha)))
    (if (< nr 10)
        (let ((vec (cond
                    ((<= alpha .01)
                     '#(0 0 0 0 0 0 0 0 0 36 43))
                    ((<= alpha .02)
                     '#(0 0 0 0 0 0 0 0 28 34 39))
                    ((<= alpha .05)
                     '#(0 0 0 0 0 0 0 21 24 30 35))
                    ((<= alpha .10)
                     '#(0 0 0 0 0 0 15 17 22 26 29))
                    (else (error "unsupported alpha" alpha)))))
          (if (< nr (vector-length vec))
              (vector-ref vec nr)
              (error "wilcoxon critical table incomplete" nr)))
        (inverse-normal-cdf (- 1 alpha)))))

;; anova

;;> Performs an f-test of equality of variance of the distributions.
;;> Returns the p-value.
(define (f-test seq-of-seqs)
  (let* ((total-length
          (fold (lambda (s acc) (+ (seq-length s) acc)) 0 seq-of-seqs))
         (df-total (- total-length 1))
         (df-between (- (seq-length seq-of-seqs) 1))
         (df-within (- df-total df-between)))
    (- 1 (f-cdf (f-statistic seq-of-seqs)
                df-between df-within))))

;;> Returns \scheme{#t} iff the \var{f-test} is less than \var{alpha}.
(define (f-test? seq-of-seqs . o)
  (let ((alpha (if (pair? o) (car o) 0.05)))
    (< (f-test seq-of-seqs) alpha)))

;;> Returns the underlying statistic for \scheme{f-test}.
(define (f-statistic seq-of-seqs)
  ;; TODO: two-way, include Error
  (let* ((total-length
          (fold (lambda (s acc) (+ (seq-length s) acc)) 0 seq-of-seqs))
         (cf (/ (square (fold (lambda (s acc) (+ (sum s) acc)) 0 seq-of-seqs))
                total-length))
         (total-ss (- (fold (lambda (s acc) (+ (square-sum s) acc)) 0 seq-of-seqs)
                      cf))
         (between-ss (- (/ (fold (lambda (s acc)
                                   (+ (square (sum s)) acc))
                                 0
                                 seq-of-seqs)
                           4)
                        cf))
         (within-ss (abs (- total-ss between-ss)))
         (df-total (- total-length 1))
         (df-between (- (seq-length seq-of-seqs) 1))
         (df-within (- df-total df-between))
         (between-ms (/ between-ss df-between))
         (within-ms (/ within-ss df-within)))
    (/ between-ms within-ms)))

;;> \subsection{Confidence Intervals}

;;> Returns the lower bound of the Wilson score interval, a binomial
;;> proportion confidence interval.  This interval gives a better
;;> understanding of the probability of success from a series of
;;> Bernoulli trials than the raw ratio, which can be misleading for a
;;> small number of trials.
(define (wilson-lower-bound successes trials . o)
  ;; TODO: consider contuinity correction
  (let* ((confidence (if (pair? o) (car o) 0.95))
         (z (inverse-normal-cdf confidence)))
    (- (/ (+ successes (/ (square z) 2))
          (+ trials (square z)))
       (wilson-score-offset successes trials z))))

;;> Returns the upper bound of the Wilson score interval.
(define (wilson-upper-bound successes trials . o)
  (let* ((confidence (if (pair? o) (car o) 0.95))
         (z (inverse-normal-cdf confidence)))
    (+ (/ (+ successes (/ (square z) 2))
          (+ trials (square z)))
       (wilson-score-offset successes trials z))))

;;> Returns the offset used in \scheme{wilson-lower-bound} and
;;> \scheme{wilson-upper-bound}.
(define (wilson-score-offset successes trials . o)
  (let ((z (if (pair? o) (car o) 1.96)))
    (* (/ z (+ trials (square z)))
       (sqrt (+ (/ (* successes (- trials successes))
                   trials)
                (/ (square z) 4))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{Similarity}

(define (intersection-size seq1 seq2 . o)
  (let ((s (seq-fold (lambda (set elt) (set-adjoin! set elt))
                     (set (if (pair? o) (car o) the-equal-comparator))
                     seq1)))
    (seq-fold (lambda (n elt)
                (if (set-contains? s elt)
                    (+ n 1)
                    n))
              0
              seq2)))

;;> A similarity measure, also known as the Jaccard similarity
;;> coefficient, returns the size of the intersection of \var{seq1}
;;> and \var{seq2} divided by the size of their union. The range is in
;;> [0, 1], with 0 indicating no intersection and 1 indicating they
;;> are the same sets.  Equality is determined by the comparator
;;> \var{cmp}, defaulting to an \scheme{equal?} comparator.
(define (jaccard-index dist1 dist2 . o)
  (let* ((cmp (if (pair? o) (car o) the-equal-comparator))
         (intersect-size (intersection-size (distribution-values dist1)
                                            (distribution-values dist2)
                                            cmp))
         (union-size (- (+ (size dist1) (size dist2))
                        intersect-size)))
    (/ intersect-size
       union-size)))

;;> Returns the dissimilarity between \var{seq1} and \var{seq2}, this
;;> is 1 - the index.
(define (jaccard-distance dist1 dist2 . o)
  (- 1 (apply jaccard-index dist1 dist2 o)))

;;> The Sørensen-Dice coefficient, aka Czekanowski's binary index,
;;> Zijdenbos similarity index.  Another similarity index, this is
;;> essentially the set equivalent of the \scheme{harmonic-mean}.
;;>
;;> The corresponding distance function (subtracted from 1), unlike
;;> Jaccard, is not a proper distance metric because it doesn't
;;> satisfy the triangle inequality.
(define (sorenson-dice-index seq1 seq2 . o)
  (/ (* 2 (apply intersection-size seq1 seq2 o))
     (+ (seq-length seq1)
        (seq-length seq2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{Simple Linear Regression}

;;> Computes a simple linear regression using ordinary least squares
;;> on the paired sequences \var{x} and \var{y} and returns two
;;> values: the y-intercept and the slope of the line closest fitting
;;> the points.  This is the univariate case and so simply called
;;> \var{fit-line} to distinguish from the multivariate linear
;;> regression used in machine learning.
;;>
;;> Example: The number of SRFIs has been increasing at a steady rate
;;> in the past 20 years.  Assuming the growth is linear, how many
;;> final SRFIs do we expect in 2030?
;;>
;;> \example{
;;> (let ((srfis '((1999 9) (2000 16) (2001 19) (2002 30) (2003 34)
;;>                (2004 42) (2005 58) (2006 62) (2007 67) (2008 71)
;;>                (2009 72) (2010 73) (2012 74) (2013 82) (2014 85)
;;>                (2015 92) (2016 108) (2017 119) (2018 124)
;;>                (2019 138) (2020 151))))
;;>   (call-with-values (lambda () (fit-line (map car srfis) (map cadr srfis)))
;;>     (lambda (m b)
;;>       (let ((f (lambda (x) (inexact (+ (* m x) b)))))
;;>         (f 2030)))))}
;;>
;;> Given only 73 SRFIs over the first decade this may seem
;;> reasonable, but this doesn't account for the recent explosion in
;;> activity, with over 200 SRFIs counting those in draft.  If the
;;> relation is not linear, \scheme{fit-line} is a bad tool.
(define (fit-line x y)
  (let ((n (seq-length x)))
    (unless (= n (seq-length y))
      (error "inputs must have same length" x y))
    (let* ((sx (sum x))
           (sxx (square-sum x))
           (sy (sum y))
           (syy (square-sum y))
           (sxy (seq-fold (lambda (sum xi yi) (+ sum (* xi yi))) 0 x y))
           (b (/ (- (* n sxy) (* sx sy))
                 (- (* n sxx) (square sx))))
           (a (/ (- sy (* b sx)) n))
           (se (/ (- (* n syy)
                     (square sy)
                     (* b b (- (* n sxx) (square sx))))
                  n
                  (- n 2)))
           (sb (/ (* n se) (- (* n sxx) (square sx))))
           (sa (/ (* sb sxx) n)))
      (values b a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{Utilities}

;;> Returns \var{x} raised to the 3rd power.
(define (cube x)
  (* x x x))

;;> Returns \var{x} raised to the 4th power.
(define (tesseract x)
  (* x x x x))

;;> Returns the sign of \var{x}, either 0, 1 or -1.
(define (sign x)
  (cond
   ((zero? x) 0)
   ((positive? x) 1)
   (else -1)))

;;> Returns \var{n}! = 1*2*...*\var{n}.
(define (factorial n)
  (let fact ((n n) (res 1))
    (if (<= n 1) res (fact (- n 1) (+ n res)))))

;;> Returns the log of the gamma function applied to \var{x},
;;> i.e. just the first value of \scheme{flloggamma} from SRFI 144.
(define (loggamma x)
  (call-with-values (lambda () (flloggamma x))
    (lambda (res sign) res)))

;;> Returns the regularized lower incomplete gamma, γ(\vars}, \var{z}.
(define (lower-incomplete-gamma s z)
  (let lp ((k 1) (x 1.0) (sum 1.0))
    (if (or (= k 1000) (< (/ x sum) 1e-14))
        (exp (+ (* s (log z))
                (log sum)
                (- z)
                (- (loggamma (+ s 1.)))))
        (let* ((x2 (* x (/ z (+ s k))))
               (sum2 (+ sum x2)))
          (lp (+ k 1) x2 sum2)))))

;;> The probability distribution function for the beta distribution.
(define (beta-pdf a b x)
  (/ (* (expt x (- a 1))
        (expt (- 1 x) (- b 1)))
     (/ (* (flgamma a) (flgamma b))
        (flgamma (+ a b)))))

;;> Returns the incomplete beta function, Β(\var{a}, \var{b}).
(define (beta a b . o)
  (define epsilon 1e-30)
  (define delta 1e-8)
  (define limit 200)
  (define (abs-max a b)
    (if (< (abs a) (abs b)) b a))
  (let ((x (if (pair? o) (car o) 1)))
    (cond
     ((= x 1)
      (exp (+ (loggamma (inexact a))
              (loggamma (inexact b))
              (- (loggamma (inexact (+ a b)))))))
     ((not (< 0 x 1))
      +inf.0)
     ((> x (/ (+ a 1) (+ a b 2)))
      (- 1 (beta b a (- 1 x))))
     (else
      ;; Lentz's algorithm to solve the continued fraction.
      (let lp ((i 0) (f 1) (c 1) (d 0))
        (cond
         ((>= i limit)
          +inf.0)
         ((< (abs (- 1 (* c d))) delta)
          (* (/ (exp (+ (* a (log x))
                        (* b (log (- 1 x)))
                        (- (loggamma (inexact (+ a b)))
                           (loggamma (inexact a))
                           (loggamma (inexact b)))))
                a)
             (- f 1)))
         (else
          (let* ((m (quotient i 2))
                 (num (cond ((zero? i) 1)
                            ((even? i)
                             (/ (* m (- b m) x)
                                (* (+ a (* 2 m) -1)
                                   (+ a (* 2 m)))))
                            (else
                             (- (/ (* (+ a m) (+ a b m) x)
                                   (* (+ a (* 2 m))
                                      (+ a (* 2 m) 1)))))))
                 (c (abs-max (+ 1 (/ num c)) epsilon))
                 (d (/ (abs-max (+ 1 (* num d)) epsilon))))
            (lp (+ i 1) (* f c d) c d)))))))))

;;> Returns the quantile function, or inverse distribution function,
;;> for the given \var{cdf}, with \var{statistics} used to determine
;;> the domain.
(define (inverse-transform-cdf cdf statistics)
  (define (finite-lower-bound f x lo)
    (inexact
     (if (finite? lo)
         lo
         (let lp ((lo -1)) (if (< (cdf lo) x) lo (lp (* lo 10)))))))
  (define (finite-upper-bound f x hi)
    (inexact
     (if (finite? hi)
         hi
         (let lp ((hi 1)) (if (> (cdf hi) x) hi (lp (* hi 10)))))))
  (lambda (x)
    (let lp ((lo (finite-lower-bound cdf x (statistics-minimum statistics)))
             (hi (finite-upper-bound cdf x (statistics-maximum statistics)))
             (count 0))
      (let* ((mid (/ (+ lo hi) 2))
             (x^ (cdf mid)))
        (cond
         ((or (= x^ x) (> count 63)) mid)
         ((< x^ x) (lp mid hi (+ count 1)))
         (else (lp lo mid (+ count 1))))))))

;;> Utility to return a random value matching a given \var{cdf}, with
;;> \var{statistics} used to determine the domain.
(define (inverse-transform-random cdf statistics)
  (let ((inverse-cdf (inverse-transform-cdf cdf statistics)))
    (lambda ()
      (inverse-cdf (random-real)))))

;;> Returns a procedure of one value which approximates the derivative
;;> of \var{f}.
(define (numeric-diff f)
  (lambda (x) (/ (- (f (+ x .001)) (f (- x .001))) (* 2 .001))))

;;> Also known as the binomial coefficient, returns the number of
;;> combinations of \var{k} elements from a set of size \var{n}.
(define (combinations n k)
  (let lp ((n n) (num 1) (i 1) (den 1))
    (if (> i k)
        (/ num den)
        (lp (- n 1) (* n num) (+ i 1) (* i den)))))

;;> Returns the number of permutations of \var{k} ordered elements
;;> from a set of size \var{n}.
(define (permutations n k)
  (* (combinations n k) (factorial k)))

(define the-equal-comparator (make-equal-comparator))

(define-record-type Running-Sum
  (%make-running-sum sum compensation)
  running-sum?
  (sum running-sum-sum running-sum-sum-set!)
  (compensation running-sum-compensation running-sum-compensation-set!))

;;> Returns the sum of the elements of \var{seq}, which must be real.
;;> This and all operations involving sums are numerically stable.
;;> The current implementation uses Neumaier's variant of Kahan
;;> summation.
;;>
;;> Examples:
;;> \example{(sum (make-list 10000 (acos -1)))}
;;> \example{(sum '#(1 1e100 1 -1e100))}
(define (sum seq . o)
  (if (pair? o)
      (map-sum (car o) seq)
      (if (vector? seq)
          (let lp ((i (- (vector-length seq) 1)) (sum 0) (c 0))
            (if (negative? i)
                (+ sum c)
                (let* ((elt (vector-ref seq i))
                       (t (+ sum elt)))
                  (if (>= (abs sum) (abs elt))
                      (lp (- i 1) t (+ c (+ (- sum t) elt)))
                      (lp (- i 1) t (+ c (+ (- elt t) sum)))))))
          (let lp ((ls seq) (sum 0) (c 0))
            (if (null? ls)
                (+ sum c)
                (let* ((elt (car ls))
                       (t (+ sum elt)))
                  (if (>= (abs sum) (abs elt))
                      (lp (cdr ls) t (+ c (+ (- sum t) elt)))
                      (lp (cdr ls) t (+ c (+ (- elt t) sum))))))))))

;;> Like \scheme{sum} but sums the \scheme{square}s of the elements.
(define (square-sum seq . o)
  (if (pair? o)
      (map-sum (lambda (x) (square ((car o) x))) seq)
      (map-sum square seq)))

;;> Like \scheme{sum} but sums the inverse (\scheme{/}) of the elements.
(define (reciprocal-sum seq . o)
  (if (pair? o)
      (map-sum (lambda (x) (/ ((car o) x))) seq)
      (map-sum (lambda (x) (/ x)) seq)))

;;> Like \scheme{sum} but sums the \scheme{log} of the elements.
(define (log-sum seq . o)
  (if (pair? o)
      (map-sum (lambda (x) (log ((car o) x))) seq)
      (map-sum log seq)))

;;> Generalized stable \scheme{sum}.  Applies \var{proc} to the
;;> sequences and returns the sum of all the results.  It is an error
;;> if the sequences don't all have the same length.
(define (map-sum proc seq . o)
  (if (pair? o)
      ;; TODO: more than 2 seqs
      (let ((len1 (seq-length seq))
            (len2 (seq-length (car o))))
        (unless (= len1 len2)
          (error "sequences must have equal length" seq (car o)))
        (do ((i 0 (+ i 1))
             (res (make-running-sum)
                  (let ((e1 (if (pair? seq1) (car seq1) (seq-ref seq i)))
                        (e2 (if (pair? seq2) (car seq2) (seq-ref (car o) i))))
                    (running-sum-inc! res (proc e1 e2))))
             (seq1 seq (if (pair? seq1) (cdr seq1) '()))
             (seq2 (car o) (if (pair? seq2) (cdr seq2) '())))
            ((= i len1)
             (running-sum-get res))))
      (if (vector? seq)
          (let lp ((i (- (vector-length seq) 1)) (sum 0) (c 0))
            (if (negative? i)
                (+ sum c)
                (let* ((elt (proc (vector-ref seq i)))
                       (t (+ sum elt)))
                  (if (>= (abs sum) (abs elt))
                      (lp (- i 1) t (+ c (+ (- sum t) elt)))
                      (lp (- i 1) t (+ c (+ (- elt t) sum)))))))
          (let lp ((ls seq) (sum 0) (c 0))
            (if (null? ls)
                (+ sum c)
                (let* ((elt (proc (car ls)))
                       (t (+ sum elt)))
                  (if (>= (abs sum) (abs elt))
                      (lp (cdr ls) t (+ c (+ (- sum t) elt)))
                      (lp (cdr ls) t (+ c (+ (- elt t) sum))))))))))

;;> Creates a new running-sum object initialized to \var{init},
;;> defaulting to 0.  A running-sum allows you take compute a
;;> numerically stable sum incrementally, so that it can be updated
;;> over time or fed from a generator or other data structure, etc.
(define (make-running-sum . o)
  (let ((init (if (pair? o) (car o) 0)))
    (%make-running-sum init 0)))

;;> \procedure{(running-sum? x)}
;;> Returns true iff \var{x} is a running-sum object.

;;> Returns the current value of the running-sum \var{rs}.
(define (running-sum-get rs)
  (+ (running-sum-sum rs)
     (running-sum-compensation rs)))

;;> Adds \var{x} to the running-sum \var{rs}.
(define (running-sum-inc! rs x)
  (let* ((sum (running-sum-sum rs))
         (t (+ sum x))
         (c (running-sum-compensation rs)))
    (if (>= (abs (running-sum-sum rs)) (abs x))
        (running-sum-compensation-set! rs (+ c (+ (- sum t) x)))
        (running-sum-compensation-set! rs (+ c (+ (- x t) sum))))
    (running-sum-sum-set! rs t)
    rs))

;;> Subtracts \var{x} from the running-sum \var{rs}.
(define (running-sum-dec! rs x)
  (running-sum-inc! rs (- x)))

;;> Returns the product of the elements in \var{seq}.  Multiplication
;;> is stable so there is no need for error compensation.
(define (product seq . o)
  (if (pair? o)
      (seq-fold (lambda (acc x) (* ((car o) x) acc)) 1 seq)
      (seq-fold * 1 seq)))

;;> The relative change from \var{x} to \var{y}, useful to answer the
;;> question what percentage the gain (or loss) was.
;;>
;;> Examples:
;;>
;;> \example{(gain 50 60)}
;;> \example{(gain 2/3 1)}
;;> \example{(gain .75 .50)}
(define (gain x y)
  (if (zero? x)
      y
      (/ (- y x) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{Additional Data Structures}

;;> We make use of reservoirs and histograms for summarizing
;;> distributions.

;;> \subsection{Reservoirs}

;;> Returns a new reservoir, a random running sample of up to
;;> \var{max-size} elements.  As new elements are added beyond
;;> \var{max-size}, the elements are probabilistic replaced such that
;;> the sample has the same distribution as if \scheme{random-sample}
;;> was run on the full population.
(define (make-reservoir max-size)
  (%make-reservoir (make-vector (max max-size 128) #f) max-size 0))

;;> Probabilistically adds \var{elt} to the reservoir \var{rv}.
(define (reservoir-maybe-add! rv elt)
  (cond
   ((< (reservoir-count rv)
       (vector-length (reservoir-elements rv)))
    (vector-set! (reservoir-elements rv)
                 (reservoir-count rv)
                 elt))
   ((< (reservoir-count rv)
       (reservoir-max-size rv))
    (let ((elts (make-vector
                 (min (* 2 (vector-length (reservoir-elements rv)))
                      (reservoir-max-size rv))
                 #f)))
      (vector-copy! elts 0 (reservoir-elements rv))
      (vector-set! elts (reservoir-count rv) elt)
      (reservoir-elements-set! rv elts)))
   ((< (random-real)
       (/ (reservoir-max-size rv) (reservoir-count rv)))
    (vector-set! (reservoir-elements rv)
                 (random-integer (reservoir-max-size rv))
                 elt)))
  (reservoir-count-set! rv (+ 1 (reservoir-count rv))))

;;> Returns a vector containing the current elements of the reservoir
;;> \var{rv}, which may share space with the underlying data structure.
(define (reservoir->vector! rv)
  (let ((elts (reservoir-elements rv))
        (size (reservoir-count rv)))
    (if (< count (vector-length elts))
        (vector-copy elts 0 count)
        elts)))

;;> \subsection{Histograms}

;; Trivial implementation based on:
;;   http://jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf
;; for more sophisticated techniques see:
;;   http://www.mathcs.emory.edu/~cheung/papers/StreamDB/Histogram/2005-Guha-Histogram.pdf

;;> Returns a new empty histogram for describing a distribution using
;;> up to \var{max-bins} bins.
(define (make-histogram . o)
  (let ((max-bins (if (pair? o) (car o) 100)))
    (%make-histogram '() max-bins 0)))

;;> \procedure{(histogram? x)}
;;> Returns \scheme{#t} iff \var{x} is a histogram.

;;> Adds an element to a histogram.
(define (histogram-add! hist elt)
  (histogram-count-set! hist (+ 1 (histogram-count hist)))
  (cond
   ((null? (histogram-bins hist))
    (histogram-bins-set! hist (list (cons elt 1))))
   ((< elt (caar (histogram-bins hist)))
    (histogram-bins-set! hist (cons (cons elt 1) (histogram-bins hist)))
    (histogram-trim! hist))
   (else
    (let lp ((ls (histogram-bins hist)))
      (cond
       ((= elt (caar ls))
        (set-cdr! (car ls) (+ 1 (cdar ls))))
       ((or (null? (cdr ls)) (< elt (caar (cdr ls))))
        (set-cdr! ls (cons (cons elt 1) (cdr ls)))
        (histogram-trim! hist))
       (else
        (lp (cdr ls))))))))

;;> Returns a new histogram summarizing the elements of list \var{ls}.
(define (list->histogram ls . o)
  (let* ((max-bins (if (pair? o) (car o) 100))
         (res (make-histogram max-bins)))
    (for-each (lambda (x) (histogram-add! res x)) ls)
    res))

;;> Returns a new histogram summarizing the elements of vector \var{vec}.
(define (vector->histogram vec . o)
  (let* ((max-bins (if (pair? o) (car o) 100))
         (res (make-histogram max-bins)))
    (vector-for-each (lambda (x) (histogram-add! res x)) vec)
    res))

(define (histogram-trim! hist)
  (define (assoc-closest ls)
    (let lp ((ls ls) (closest ls) (diff +inf.0))
      (if (or (null? ls) (null? (cdr ls)))
          closest
          (let ((diff2 (abs (- (caar ls) (caar (cdr ls))))))
            (if (< diff2 diff)
                (lp (cdr ls) ls diff2)
                (lp (cdr ls) closest diff))))))
  (when (> (length (histogram-bins hist)) (histogram-max-bins hist))
    (let* ((ls (assoc-closest (histogram-bins hist)))
           (a (car ls))
           (b (cadr ls))
           (total (+ (cdr a) (cdr b)))
           (avg (/ (+ (* (car a) (cdr a)) (* (car b) (cdr b))) total)))
      (set-car! ls (cons avg total))
      (set-cdr! ls (cddr ls)))))

;;> Returns the smallest value seen by the histogram \var{hist}.
(define (histogram-min hist)
  (caar (histogram-bins hist)))

;;> Returns the largest value seen by the histogram \var{hist}.
(define (histogram-max hist)
  (car (last (histogram-bins hist))))

(define (histogram-scale hist . o)
  (if (null? (histogram-bins hist))
      '#()
      (let* ((num-bins (if (pair? o) (car o) (length (histogram-bins hist))))
             (o (if (pair? o) (cdr o) '()))
             (lo (if (pair? o) (car o) (histogram-min hist)))
             (o (if (pair? o) (cdr o) '()))
             (hi (if (pair? o) (car o) (histogram-max hist)))
             (width (/ (- hi lo) num-bins))
             (res (make-vector num-bins 0)))
        (do ((ls (histogram-bins hist) (cdr ls)))
            ((null? ls) res)
          (let ((i (min (- num-bins 1)
                        (max 0 (exact (floor (/ (- (caar ls) lo) width)))))))
            (vector-set! res i (+ (cdar ls) (vector-ref res i))))))))

;;> Returns the \var{x}th quantile ([0, 1]) of the histogram \var{hist}.
(define (histogram-quantile hist x)
  ;; TODO: interpolate quantile between bins
  (let ((size (* x (histogram-count hist))))
    (let lp ((ls (histogram-bins hist))
             (c 0))
      (cond ((null? ls) -1)
            ((>= (+ c (cdar ls)) count) (caar ls))
            (else (lp (cdr ls) (+ c (cdar ls))))))))

;;> Returns the \var{x}th decile ([0, 10]) of the histogram \var{hist}.
(define (histogram-deciles hist)
  (map (lambda (x) (histogram-quantile hist (* x 0.1)))
       (iota 10)))

;;> Simple utility to plot a histogram with characters in a
;;> fixed-width font.
(define (histogram-plot-ascii hist . o)
  (let* ((bins (histogram-bins hist))
         (width (if (pair? o) (car o) (max 78 (length bins))))
         (height (if (and (pair? o) (pair? (cdr o))) (cadr o) 40))
         (limit (fold (lambda (bin acc) (max (cdr bin) acc)) 0 bins)))
    (if (<= (length bins) width)
        (do ((i (- height 1) (- i 1)))
            ((< i 0))
          (write-string
           (list->string
            (map (lambda (bin)
                   (if (> (cdr bin) (* limit (/ i height))) #\* #\space))
                 bins)))
          (newline))
        (error "bin scaling not supported, use fewer bins or more width"))))

;;> Computes the cumulative distribution function of \var{hist} at
;;> point \var{x}.
(define (histogram-cdf hist x)
  (let lp ((ls (histogram-bins hist))
           (c 0))
    (cond ((null? ls) 1)
          ((> (caar ls) x) (/ c (histogram-count hist)))
          (else (lp (cdr ls) (+ c (cdar ls)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;> \section{References}
;;>
;;> \itemlist[
;;> \item{\hyperlink[https://www-cs-faculty.stanford.edu/~knuth/taocp.html]{The Art of Computer Programming, Volume 2: Seminumerical Algorithms}, Donald E. Knuth (1997)}
;;> \item{\hyperlink[https://cran.r-project.org/doc/manuals/r-release/R-intro.html]{An Introduction to R}, R Core Team (1999-2018)}
;;> \item{\hyperlink[http://homepage.divms.uiowa.edu/~luke/xls/tutorial/techreport/techreport.html]{XLISP-STAT}, Luke Tierney (1989)}
;;> \item{\hyperlink[https://commons.apache.org/proper/commons-math/userguide/stat.html]{Apache Commons Math - Statistics}, The Apache Software Foundation (2003-2016)}
;;> \item{\hyperlink[http://www.jstatsoft.org/v08/i18/paper]{Evaluating Kolmogorov’s Distribution}, George Marsaglia et al (2003)}
;;> \item{\hyperlink[https://srfi.schemers.org/srfi-144/srfi-144.html]{SRFI 144 - Flonums}, John Cowan and Will Clinger (2017)}
;;> \item{\hyperlink[https://www.mat.univie.ac.at/~neum/scan/01.pdf]{Rounding Error Analysis of Some Methods for Summing Finite Sums}, Arnold Neumaier (1974)}
;;> \item{\hyperlink[https://jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf]{A Streaming Parallel Decision Tree Algorithm}, Yael Ben-Haim et al (2010)}
;;> \item{\hyperlink[http://www.johnmyleswhite.com/notebook/2013/03/22/modes-medians-and-means-an-unifying-perspective/]{Modes, Medians and Means: A Unifying Perspective}, John Myles White (2013)}
;;> \item{\hyperlink[https://towardsdatascience.com/on-average-youre-using-the-wrong-average-geometric-harmonic-means-in-data-analysis-2a703e21ea0]{On Average, You’re Using the Wrong Average}, Daniel McNichol (2018)}
;;> ]
