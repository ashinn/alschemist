
;;> Statistics is the branch of mathematics dealing with the collection
;;> and analysis of data.  As we are increasingly overwhelmed with data
;;> it is important to be able to analyze that data, and so statistics
;;> should have a prominant role in any programming language's standard
;;> libraries.

;;> Broadly speaking statistics can be divided into descriptive
;;> statistics and inferential statistics.

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

(define (seq-stdev seq . o)
  (sqrt (apply seq-variance seq o)))

(define (seq-maximum seq . o)
  (let ((less (if (pair? o) (car o) <)))
    (seq-fold (lambda (a b) (if (less a b) b a)) -inf.0 seq)))

(define (seq-minimum seq . o)
  (let ((less (if (pair? o) (car o) <)))
    (seq-fold (lambda (a b) (if (less b a) b a)) +inf.0 seq)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define (cube x)
  (* x x x))

(define (tesseract x)
  (* x x x x))

(define (sign x)
  (cond
   ((zero? x) 0)
   ((positive? x) 1)
   (else -1)))

(define (factorial n)
  (let fact ((n n) (res 1))
    (if (<= n 1) res (fact (- n 1) (+ n res)))))

(define (loggamma x)
  (call-with-values (lambda () (flloggamma x))
    (lambda (res sign) res)))

;; regularized
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

(define (beta-pdf a b x)
  (/ (* (expt x (- a 1))
        (expt (- 1 x) (- b 1)))
     (/ (* (flgamma a) (flgamma b))
        (flgamma (+ a b)))))

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

(define (inverse-transform-random cdf statistics)
  (define (finite-lower-bound f x lo)
    (if (finite? lo)
        lo
        (let lp ((lo -1)) (if (< lo x) lo (lp (* lo 10))))))
  (define (finite-upper-bound f x hi)
    (if (finite? hi)
        hi
        (let lp ((hi 1)) (if (> hi x) hi (lp (* hi 10))))))
  (lambda ()
    (let ((x (random-real)))
      (let lp ((lo (finite-lower-bound cdf x (statistics-minimum statistics)))
               (hi (finite-upper-bound cdf x (statistics-maximum statistics)))
               (count 0))
        (let* ((mid (/ (+ lo hi) 2))
               (x^ (cdf mid)))
          (cond
           ((or (= x^ x) (> count 63)) mid)
           ((< x^ x) (lp lo mid (+ count 1)))
           (else (lp mid hi (+ count 1)))))))))

(define (numeric-diff f)
  (lambda (x) (/ (- (f (+ x .001)) (f (- x .001))) (* 2 .001))))

;; aka binomial coefficient
(define (combinations n k)
  (let lp ((n n) (num 1) (i 1) (den 1))
    (if (> i k)
        (/ num den)
        (lp (- n 1) (* n num) (+ i 1) (* i den)))))

(define (permutations n k)
  (* (combinations n k) (factorial k)))

(define the-equal-comparator (make-equal-comparator))

(define-record-type Running-Sum
  (%make-running-sum sum compensation)
  running-sum?
  (sum running-sum-sum running-sum-sum-set!)
  (compensation running-sum-compensation running-sum-compensation-set!))

(define (make-running-sum . o)
  (%make-running-sum (if (pair? o) (car o) 0) 0))

(define (running-sum-get rs)
  (+ (running-sum-sum rs)
     (running-sum-compensation rs)))

(define (running-sum-inc! rs elt)
  (let* ((sum (running-sum-sum rs))
         (t (+ sum elt))
         (c (running-sum-compensation rs)))
    (if (>= (abs (running-sum-sum rs)) (abs elt))
        (running-sum-compensation-set! rs (+ c (+ (- sum t) elt)))
        (running-sum-compensation-set! rs (+ c (+ (- elt t) sum))))
    (running-sum-sum-set! rs t)
    rs))

(define (running-sum-dec! rs x)
  (running-sum-inc! rs (- x)))

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

;; Neumaier's variant of Kahan summation
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

(define (square-sum seq . o)
  (if (pair? o)
      (map-sum (lambda (x) (square ((car o) x))) seq)
      (map-sum square seq)))

(define (reciprocal-sum seq . o)
  (if (pair? o)
      (map-sum (lambda (x) (/ ((car o) x))) seq)
      (map-sum (lambda (x) (/ x)) seq)))

(define (log-sum seq . o)
  (if (pair? o)
      (map-sum (lambda (x) (log ((car o) x))) seq)
      (map-sum log seq)))

(define (sum/fast seq)
  (seq-fold + 0 seq))

(define (square-sum/fast seq)
  (seq-fold (lambda (sum x) (+ (square x) sum)) 0 seq))

(define (log-sum/fast seq)
  (seq-fold (lambda (sum x) (+ (log x) sum)) 0 seq))

;; multiplication is stable, no need for error handling
(define (product seq . o)
  (if (pair? o)
      (seq-fold (lambda (acc x) (* ((car o) x) acc)) 1 seq)
      (seq-fold * 1 seq)))

;;> The relative change from x to y.
(define (gain x y)
  (if (zero? x)
      y
      (/ (- y x) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Distributions

;; Maintains a random running sample of up to max-size elements.
(define-record-type Reservoir
  (%make-reservoir elements max-size count)
  reservoir?
  (elements reservoir-elements reservoir-elements-set!)
  (max-size reservoir-max-size)
  (count reservoir-count reservoir-count-set!))

(define (make-reservoir max-size)
  (%make-reservoir (make-vector (max max-size 128) #f) max-size 0))

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

(define (reservoir->vector! rv)
  (let ((elts (reservoir-elements rv))
        (count (reservoir-count rv)))
    (if (< count (vector-length elts))
        (vector-copy elts 0 count)
        elts)))

;; Trivial implementation based on:
;;   http://jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf
;; for more sophisticated techniques see:
;;   http://www.mathcs.emory.edu/~cheung/papers/StreamDB/Histogram/2005-Guha-Histogram.pdf

(define-record-type Histogram
  (%make-histogram bins max-bins count)
  histogram?
  (bins histogram-bins histogram-bins-set!)
  (max-bins histogram-max-bins)
  (count histogram-count histogram-count-set!))

(define (make-histogram . o)
  (let ((max-bins (if (pair? o) (car o) 100)))
    (%make-histogram '() max-bins 0)))

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

(define (list->histogram seq . o)
  (let ((res (apply make-histogram o)))
    (seq-for-each (lambda (x) (histogram-add! res x)) seq)
    res))
(define vector->histogram list->histogram)

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

(define (histogram-min hist)
  (caar (histogram-bins hist)))

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

(define (histogram-quantile hist x)
  ;; TODO: average quantile between bins?
  (let ((count (* x (histogram-count hist))))
    (let lp ((ls (histogram-bins hist))
             (c 0))
      (cond ((null? ls) -1)
            ((>= (+ c (cdar ls)) count) (caar ls))
            (else (lp (cdr ls) (+ c (cdar ls))))))))

(define (histogram-deciles hist)
  (map (lambda (x) (histogram-quantile hist (* x 0.1)))
       (iota 10)))

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

(define (histogram-cdf hist x)
  (let lp ((ls (histogram-bins hist))
           (c 0))
    (cond ((null? ls) 1)
          ((> (caar ls) x) (/ c (histogram-count hist)))
          (else (lp (cdr ls) (+ c (cdar ls)))))))

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

(define (make-empty-statistics)
  (make-statistics #f #f #f #f #f #f #f #f #f))

(define (statistics-stdev stats)
  (sqrt (statistics-variance stats)))

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

;; A theoretical distribution.
(define (pure-distribution name pdf cdf statistics . o)
  (let ((random (if (pair? o)
                    (car o)
                    (inverse-transform-random cdf statistics))))
    (%make-dist name pdf cdf random #f statistics #f #f #t)))

;; A theoretical discrete distribution.
(define (pure-discrete-distribution name pdf cdf statistics . o)
  (let ((random (if (pair? o)
                    (car o)
                    (inverse-transform-random cdf statistics))))
    (%make-dist name pdf cdf random #f statistics #f #t #t)))

;; A single sample distribution.
(define (distribution seq)
  (%make-dist #f #f #f #f seq #f #f (seq-every exact-integer? seq) #f))

;; Equivalent to \scheme{distribution}, but records that the input
;; represents the full population rather than a subsample.
(define (population seq)
  (%make-dist #f #f #f #f seq #f #f (seq-every exact-integer? seq) #t))

;; A running sample distribution to which new elements can be added.
(define (open-distribution)
  (%make-dist #f #f #f #f (make-reservoir) #f #f #f))

;; A distribution from summmary statistics, without a sample.
(define (summary-distribution . o)
  (let-keywords o ((name #f) (size #f) (mean #f) (median #f) (mode #f)
                   (variance #f) (skew #f) (kurtosis #f)
                   (minimum #f) (maximum #f)
                   (discrete? #f) (population? #f))
    (let ((stats (make-statistics size mean median mode variance
                                  skew kurtosis minimum maximum)))
      (%make-dist name #f #f #f #f stats #f discrete? population?))))

;; A utility for a summary distribution of bernouli trials, based on
;; the number of successes out of the total number of trials.
(define (bernouli-trials successes trials)
  (assert (<= successes trials))
  (summary-distribution 'mean: (/ successes trials) 'size: trials))

;; A utility for a summary distribution where we only know the mean.
(define (mean-distribution mean)
  (summary-distribution 'mean: mean))

;; Returns a new distribution containing only the summary statistics
;; of the given distribution.
;; TODO: include an option to preserve a subsample summary
(define (summarize dist)
  (summary-distribution
   'name: (and (distribution? dist) (distribution-name dist))
   'size: (count dist)
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

(define (distribution-pure? dist)
  (and (distribution? dist) (distribution-pdf dist) #t))

(define (distribution-open? dist)
  (reservoir? (distribution-population dist)))

(define (distribution-add! dist elt)
  (when (not (distribution-open? dist))
    (error "can't add elements to a fixed distribution" dist elt))
  (summary-add! (distribution-summary dist) elt)
  (if (distribution-statistics dist)
      (distribution-statistics-set! dist #f)))

(define (distribution-values dist)
  (cond
   ((not (distribution? dist))
    dist)
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
;; Specific distributions

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

;; aka gaussian
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
     (make-statistics #f mean mean mean variance 0 0 -inf.0 +inf.0)
     (lambda ()
       (+ mean (* variance
                  (sqrt (* -2.0 (log (random-real))))
                  (cos (* 2.0 (acos -1) (random-real)))))))))

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
    (pure-distribution
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

;; aka Fisher–Snedecor distribution
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
;; Descriptive statistics

;; The size of the distribution population, which may be +inf.0 for
;; pure distributions, or #f if unknown.
(define (count dist)
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

;; http://www.johnmyleswhite.com/notebook/2013/03/22/modes-medians-and-means-an-unifying-perspective/
;; https://towardsdatascience.com/on-average-youre-using-the-wrong-average-geometric-harmonic-means-in-data-analysis-2a703e21ea0

;; arithmetic-mean, aka first moment, what people typically think
;; of as the average
(define mean
  (dist-dispatch statistics-mean statistics-mean-set! summary-mean seq-mean))

(define geometric-mean
  (dist-dispatch #f #f #f seq-geometric-mean))

;; aka F1 score
(define harmonic-mean
  (dist-dispatch #f #f #f seq-harmonic-mean))

;; aka root-mean-squared (RMS)
(define quadratic-mean
  (dist-dispatch #f #f #f seq-quadratic-mean))

(define median
  (dist-dispatch statistics-median statistics-median-set! #f seq-median))

(define mode
  (dist-dispatch statistics-mode statistics-mode-set! #f seq-mode))

;; aka second central moment
(define variance
  (dist-dispatch statistics-variance statistics-variance-set! summary-variance seq-variance))

(define (standard-deviation dist . o)
  (sqrt (apply variance dist o)))

(define stdev standard-deviation)

(define maximum
  (dist-dispatch statistics-maximum statistics-maximum-set! #f seq-maximum))

(define minimum
  (dist-dispatch statistics-minimum statistics-minimum-set! #f seq-minimum))

;; TODO: online percentile, parallel percentile
(define (percentile seq n . o)
  (let* ((less (if (pair? o) (car o) <))
         (mean (if (and (pair? o) (pair? (cdr o)))
                   (cadr o)
                   (lambda (a b) (/ (+ a b) 2))))
         (vec (if (vector? seq) (vector-copy seq) (list->vector seq)))
         (len (vector-length vec))
         (i (* len (/ n 100))))
    (cond
     ((zero? len) #f)
     (else
      (vector-separate! less vec (exact (floor i)))
      (if (or (integer? i) (>= (+ i 1) len))
          (vector-ref vec (exact (floor i)))
          (mean (vector-ref vec (exact (floor i)))
                (vector-ref vec (exact (floor (+ i 1))))))))))

;; aka relative standard deviation (RSD)
(define (coefficient-of-variation seq . o)
  (let* ((mu (if (pair? o) (car o) (mean seq)))
         (std (standard-deviation seq mu)))
    (/ std mu)))

(define (population-variance seq . o)
  (if (distribution? seq)
      (variance seq)
      (let ((mu (if (pair? o) (car o) (mean seq))))
        (/ (map-sum (lambda (x) (square (- x mu))) seq)
           (seq-length seq)))))

(define (population-standard-deviation seq . o)
  (sqrt (apply population-variance seq o)))

;; TODO: non-uniform probabilities
(define (covariance seq1 seq2 . o)
  (let ((mu1 (if (pair? o) (car o) (mean seq1)))
        (mu2 (if (and (pair? o) (pair? (cdr o))) (cadr o) (mean seq2))))
    (/ (seq-fold (lambda (sum elt1 elt2)
                   (+ sum (* (- elt1 mu1) (- elt2 mu2))))
                 0
                 seq1
                 seq2)
       (- (seq-length seq1) 1))))

;; aka correlation coefficient
(define (pearson-correlation seq1 seq2 . o)
  (/ (apply covariance seq1 seq2 o)
     (apply standard-deviation seq1 (if (pair? o) (list (car o)) '()))
     (apply standard-deviation seq2 (if (pair? o) (cdr o) '()))))

(define (spearman-rank-correlation seq1 seq2)
  (let ((len (seq-length seq1))
        (r1 (map-rank seq1 <))
        (r2 (map-rank seq2 <)))
    (if (and (every exact-integer? r1)
             (every exact-integer? r2))
        (- 1 (/ (* 6 (square-sum (seq-map - r1 r2)))
                (* len (- (square len) 1))))
        (pearson-correlation r1 r2))))

;; aka third central moment, the lopsidedness of the distribution
(define (skewness seq . o)
  (let* ((mu (if (pair? o) (car o) (mean seq)))
         (s3 (cube (standard-deviation seq mu))))
    (/ (map-sum (lambda (x) (cube (- x mu))) seq)
       (seq-length seq)
       s3)))

;; aka fourth central moment, the heaviness of the tail
(define (kurtosis seq . o)
  (let* ((mu (if (pair? o) (car o) (mean seq)))
         (s4 (tesseract (standard-deviation seq mu))))
    (/ (map-sum (lambda (x) (tesseract (- x mu))) seq)
       (seq-length seq)
       s4)))

(define (excess-kurtosis seq . o)
  (- (apply kurtosis seq o) 3))

;; TODO: coskewness and cokurtosis

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Similarity

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

;; similarity measure, intersection/union in [0, 1]
(define (jaccard-index seq1 seq2 . o)
  (let* ((intersect-size
          (apply intersection-size seq1 seq2 o))
         (union-size (- (+ (seq-length seq1)
                           (seq-length seq2))
                        intersect-size)))
    (/ intersect-size
       union-size)))

(define (jaccard-distance seq1 seq2 . o)
  (- 1 (apply jaccard-index seq1 seq2 o)))

;; Sørensen, aka Czekanowski's binary index, Zijdenbos similarity index.
;; Unlike Jaccard, is not a proper distance metric (doesn't satisfy
;; triangle inequality)
(define (sorenson-dice-index seq1 seq2 . o)
  (/ (* 2 (apply intersection-size seq1 seq2 o))
     (+ (seq-length seq1)
        (seq-length seq2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sampling

(define (flip? . o)
  (< (random-real) (if (pair? o) (car o) 0.5)))

(define (random-pick dist)
  (seq-ref (random-sample 1 dist) 0))

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

;; with replacement
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
   ((distribution? x)
    (let ((res (make-vector n)))
      (do ((i 0 (+ i 1)))
          ((= i n) res)
        (vector-set! res i ((distribution-random x))))))
   (else
    (let* ((seq x)
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

;;> (min-sample-size num-total [conf err p])
;;>
;;> Compute the minimum sample size for a population num-total, with
;;> the given confidence, error bound requirements, and population
;;> proportion.
;;>
;;> num-total: total population size
;;>            (+inf.0 uses the normal approximation to binomial dist, otherwise
;;>             we use the normal approximation to the hypergeometric dist)
;;> conf: desired level of confidence, default 1.96 (95%)
;;> err: desired error bounds, default 3%
;;> p: population proportion, default 0.5
;;>
;;> For example, assume you want a 99% confidence (conf: 2.75) that the
;;> result is within 1%, then:
;;>
;;>   (min-sample-size +inf.0 2.75 0.01) => 18906.25
;;>
;;> however if you know the total population the required size may be
;;> smaller (but never larger):
;;>
;;>   (min-sample-size 73015 2.75 0.01)  => 15017.80
;;>
;;> Details:
;;> http://uregina.ca/~morrisev/Sociology/Sampling%20from%20small%20populations.htm

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
;; Hypothesis testing

;; requires knowing the mean and stdev of the population
(define (standard-score x mu . o)
  (let ((sigma (if (pair? o) (car o) 1)))
    (/ (- x mu) sigma)))

;;> Performs a Z-test, a test that a \var{sample} can be approximated
;;> by a normal distribution.  Returns the probability that
;;> \var{sample} is consistent with \var{dist}, default one-tailed.
;;> This is more convenient than the T-test if the population size is
;;> large or the variance is known.
(define (z-test sample dist . o)
  (let ((tails (if (pair? o) (car o) 1))
        (stat (z-statistic sample dist)))
    (assert (and (exact-integer? tails) (<= 1 tails 2)))
    (* tails (normal-cdf (- (abs stat)) 0 1))))

;;> Returns true iff the \scheme{z-test} for \var{sample} and
;;> \var{dist} meets the given \var{alpha} level, default 5%.
(define (z-test? sample dist . o)
  (let ((alpha (if (pair? o) (car o) 0.05))
        (tails (if (and (pair? o) (pair? (cdr o))) (cadr o) 1)))
    (<= (z-test sample dist tails) alpha)))

;;> Returns the underlying Z statistic showing how well \var{dist}
;;> approximates \var{sample}.
(define (z-statistic sample dist)
  (if (not (eq? 'normal (distribution-name dist)))
      (error "z-statistic expected a normal distribution" dist))
  (* (sqrt (count sample))
     (standard-score (mean sample) (mean dist) (variance dist))))

;;> AKA the Student's t-test, after the pen name "Student" used by
;;> William Sealy Gosset.  Returns the probability that the two
;;> distributions \var{dist1} and \var{dist2} have the same mean,
;;> under the assumption the underlying distributions are normal.
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
(define (t-test? dist1 dist2 . o)
  (let ((alpha (if (pair? o) (car o) 0.05))
        (tails (if (and (pair? o) (pair? (cdr o))) (cadr o) 1)))
    (< (t-test dist1 dist2 tails) alpha)))

;;> Returns the underlying t statistic showing how well \var{dist1}
;;> matches \var{dist2}.
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
          (sqrt (count dist1)))))
   (else
    (let* ((m1 (mean dist1))
           (m2 (mean dist2))
           (s1 (standard-deviation dist1))
           (s2 (standard-deviation dist2))
           (n1 (count dist1))
           (n2 (count dist2)))
      (if (< 1/2 (/ s1 s2) 2)
          (/ (- m1 m2)
             (* (harmonic-mean n1 n2)
                (sqrt (/ (+ (* (- n1 1) (square s1))
                            (* (- n2 1) (square s2)))
                         (+ n1 n2 -2)))))
          ;; Welch's t-test (unequal variance)
          (/ (- m1 m2)
             (sqrt (+ (/ (square s1) n1)
                      (/ (square s2) n2)))))))))

;;> Returns the degrees of freedom for the \scheme{t-test} comparing
;;> \var{dist1} and \var{dist2}.
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
    (- (count dist1) 1))
   (else
    (let* ((s1 (standard-deviation dist1))
           (s2 (standard-deviation dist2))
           (s1^2 (square s1))
           (s2^2 (square s2))
           (n1 (count dist1))
           (n2 (count dist2)))
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

;; Pearson's chi-squared test.
;;
;; It involes binning the results and comparing the frequencies with
;; the expected frequencies of a theoretical distribution or the
;; observed frequencies of another sample.
(define chi^2-statistic
  (paired-dispatch
   (lambda (observed expected)
     (map-sum (lambda (o e) (if (zero? e) 0 (/ (square (- o e)) e)))
              observed
              expected))))

;; goodness of fit - test difference from theoretical dist
(define (chi^2-df observed . o)
  (let ((params (if (pair? o) (car o) 1)))
    (- (count observed) params)))

(define (chi^2-test stat df)
  (if (real? stat)
      (- 1
         (lower-incomplete-gamma
          (/ df 2)
          (/ stat 2)))
      (let ((observed stat)
            (expected df))
        (chi^2-test (chi^2-statistic observed expected)
                    (chi^2-df observed)))))

(define (chi^2-test? observed expected . o)
  (let ((alpha (if (pair? o) (car o) 0.05)))
    (< (chi^2-test observed expected) alpha)))

;; TODO: homogeneity and independence tests
;; (i.e. chi-squared over a contingency table)

(define g-statistic
  (paired-dispatch
   (lambda (observed expected)
     (* 2 (map-sum (lambda (o e) (* o (log (/ o e))))
                   observed
                   expected)))))

(define (g-test stat df)
  (if (real? stat)
      (chi^2-test stat df)
      (let ((observed stat)
            (expected df))
        (g-test (g-statistic observed expected)
                (chi^2-df observed)))))

(define (g-test? observed expected . o)
  (let ((alpha (if (pair? o) (car o) 0.05)))
    (< (g-test observed expected) alpha)))

(define (binomial-test observed expected . o)
  (let* ((tails (if (pair? o) (car o) 1))
         (n (count observed))
         (successes (* n (mean observed)))
         (p (mean expected))
         (cdf (distribution-cdf (binomial-distribution n p))))
    (assert (and (exact-integer? tails) (<= 1 tails 2)))
    (if (= tails 2)
        (let* ((diff (- p (mean observed)))
               (alt (exact (floor (+ 1 (* (+ p diff) n))))))
          (+ (- 1 (cdf successes))
             (cdf alt)))
        (- 1 (cdf successes)))))

(define (binomial-test? observed expected . o)
  (let ((tails (if (pair? o) (car o) 1))
        (alpha (if (and (pair? o) (pair? (cdr o))) (cadr o) 0.05)))
    (< (binomial-test observed expected tails) alpha)))

;; Adapted from Evaluating Kolmogorov's Distribution,
;; George Marsaglia, Wai Wan Tsang, Jingbo Wang
;; https://www.jstatsoft.org/article/view/v008i18
(define (kolmogorov-cdf n d)
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

(define (kolmogorov-smirnoff-test dist1 dist2 . o)
  (let* ((strict? (and (pair? o) (car o)))
         (seq1 (distribution-values dist1))
         (seq2 (distribution-values dist2))
         (d (kolmogorov-smirnoff-statistic seq1 seq2))
         (m (seq-length seq1))
         (n (seq-length seq2)))
    (if (< (* m n) 10000)
        ;; exact
        (- 1 (/ (lattice-paths m n m n (integrate-d d m n) strict?)
                (combinations (+ n m) m)))
        ;; approx for large tests
        (- 1 (kolmogorov-smirnoff-sum (* d (sqrt (/ (* m n) (+ m n))))
                                      ks-sum-cauchy-criterion
                                      maximum-partial-sum-count)))))

(define (kolmogorov-smirnoff-critical n m . o)
  (let ((alpha (if (pair? o) (car o) 0.05)))
    (sqrt (/  (* -1/2
                 (log (/ alpha 2))
                 (+ 1 (/ n m)))
              n))))

(define (kolmogorov-smirnoff-test? dist1 dist2 . o)
  (let* ((strict? (and (pair? o) (car o)))
         (alpha (if (pair? o) (car o) 0.05)))
    (> (kolmogorov-smirnoff-test dist1 dist2 strict?)
       (kolmogorov-smirnoff-critical (count dist1) (count dist2) alpha))))

;;> Ranks a sorted list (x1 x2 ... xn) returning
;;>   ((rank1 . x1) (rank2 . x2) ... xn)
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
(define (map-rank seq . o)
  (let* ((less (if (pair? o) (car o) <))
         (get-key (if (and (pair? o) (pair? (cdr o))) (cadr o) values))
         (get-key2 (lambda (x) (get-key (car x))))
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

;;> Wilcoxon signed ranked test.
;; (see Mann-Whitney-Wilcoxon for 2 indep samples)
;; (see sign test to remove symmetric assumption)
;; (consider modification by Pratt to incorporate zero diffs)

(define (wilcoxon-statistic dist1 dist2)
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

;; TODO: wilcoxon-test returning p-value

(define (wilcoxon-test? dist1 dist2 . o)
  (let* ((tails (if (pair? o) (car o) 1))
         (alpha (if (and (pair? o) (pair? (cdr o))) (cadr o) .05)))
    (call-with-values (lambda () (wilcoxon-statistic dist1 dist2))
      (lambda (w nr)
        (if (< nr 10)
            (< (abs w) (wilcoxon-critical nr tails alpha))
            (> (/ w (sqrt (* nr (+ nr 1) (+ (* nr 2) 1) 1/6)))
               (wilcoxon-critical nr tails alpha)))))))

;; anova
;; TODO: two-way, include Error
(define (f-statistic seq-of-seqs)
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

(define (f-test seq-of-seqs)
  (let* ((total-length
          (fold (lambda (s acc) (+ (seq-length s) acc)) 0 seq-of-seqs))
         (df-total (- total-length 1))
         (df-between (- (seq-length seq-of-seqs) 1))
         (df-within (- df-total df-between)))
    (- 1 (f-cdf (f-statistic seq-of-seqs)
                df-between df-within))))

;; wilson's confidence interval (consider contuinity correction)

(define (wilson-score-offset successes trials . o)
  (let ((z (if (pair? o) (car o) 1.96)))
    (* (/ z (+ trials (square z)))
       (sqrt (+ (/ (* successes (- trials successes))
                   trials)
                (/ (square z) 4))))))

(define (wilson-lower-bound successes trials . o)
  (let* ((confidence (if (pair? o) (car o) 0.95))
         (z (inverse-normal-cdf confidence)))
    (- (/ (+ successes (/ (square z) 2))
          (+ trials (square z)))
       (wilson-score-offset successes trials z))))

(define (wilson-upper-bound successes trials . o)
  (let* ((confidence (if (pair? o) (car o) 0.95))
         (z (inverse-normal-cdf confidence)))
    (+ (/ (+ successes (/ (square z) 2))
          (+ trials (square z)))
       (wilson-score-offset successes trials z))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simple linear regression (ordinary least squares)

;; Univariate case, just call it fit-line to distinguish from
;; multivariate linear regression used in machine learning.
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
