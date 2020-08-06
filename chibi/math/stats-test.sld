
(define-library (chibi math stats-test)
  (import (scheme base) (scheme inexact) (chibi math stats) (chibi test))
  (export run-tests)
  (begin
    (define (die-dist n)
      (discrete-uniform-distribution 1 n))
    (define (run-tests)
      (test-begin "(chibi math stats)")
      (test 31415.92653589793 (sum (make-list 10000 (acos -1))))
      (test 2.0 (sum (list 1 1e100 1 -1e100)))
      (test 2.0 (reciprocal-sum (list 1 1e100 1 -1e100)))
      (test 20000.0 (square-sum (make-list 10000 (sqrt 2))))
      (test 10000.0 (log-sum (make-list 10000 (exp 1))))
      (let ((hist (list->histogram '(3 4 6 8 5 7 10 2 1 9) 5)))
        (test 1.5 (histogram-min hist))
        (test 9.5 (histogram-max hist))
        (test 10 (histogram-count hist))
        (test '(1.5 3.5 5.5 7.5 9.5)
            (map inexact (map car (histogram-bins hist))))
        (test (make-list 5 2) (map cdr (histogram-bins hist)))
        (test '#(2 2 2 4) (histogram-scale hist 4)))
      (test-error (mean '()))
      (test 0 (mean '(0)))
      (test 5/2 (mean '(1 2 3 4)))
      (test 14/5 (mean '(1 2 3 4 4)))
      (test 3 (mean '(1 2 3 4 5)))
      (test 0.0 (- #i1/3 (mean (make-vector 10000 #i1/3))))
      (test-error (median '()))
      (test 0 (median '(0)))
      (test 5/2 (median '(1 2 3 4)))
      (test 3 (median '(1 2 3 4 4)))
      (test 3 (median '(1 2 3 4 5)))
      (test 1 (mode '(1 2 3 4 5)))
      (test 4 (mode '(1 2 3 4 4)))
      (test '() (random-sample 0 '(1 2 3 4 5)))
      (test '#() (random-sample 0 '#(1 2 3 4 5)))
      (test #t (pair? (random-sample 3 '(1 2 3 4 5))))
      (test #t (vector? (random-sample 3 '#(1 2 3 4 5))))
      (test 5 (length (random-sample 5 '(1 2 3 4 5))))
      (test 5 (vector-length (random-sample 5 '#(1 2 3 4 5))))
      (test '() (random-multi-sample 0 '(1 2 3 4 5)))
      (test '#() (random-multi-sample 0 '#(1 2 3 4 5)))
      (test #t (pair? (random-multi-sample 3 '(1 2 3 4 5))))
      (test #t (vector? (random-multi-sample 3 '#(1 2 3 4 5))))
      (test 7 (length (random-multi-sample 7 '(1 2 3 4 5))))
      (test 7 (vector-length (random-multi-sample 7 '#(1 2 3 4 5))))
      (let* ((gauss (normal-distribution))
             (sample (random-multi-sample 2000 gauss)))
        (test 2000 (vector-length sample))
        (parameterize ((current-test-epsilon 0.1))
          (test 0.0 (mean sample))
          (test 1.0 (variance sample))))
      (let* ((gauss (normal-distribution 42))
             (sample (random-multi-sample 2000 gauss)))
        (test 2000 (vector-length sample))
        (parameterize ((current-test-epsilon 0.1))
          (test 42.0 (mean sample))
          (test 1.0 (variance sample))))
      (let* ((uni (uniform-distribution 0 1))
             (sample (random-multi-sample 2000 uni)))
        (test 2000 (vector-length sample))
        (parameterize ((current-test-epsilon 0.1))
          (test 0.5 (mean sample))
          (test (/ 12.) (variance sample))))
      (let* ((p (poisson-distribution 4 5))
             (sample (random-multi-sample 2000 p)))
        (test 2000 (vector-length sample))
        (parameterize ((current-test-epsilon 0.1))
          (test 4. (mean sample))
          (test 4. (variance sample))))
      (let ((sample (summary-distribution 'mean: 96 'size: 55))
            (dist (normal-distribution 100 12)))
        (test -2.47206616 (z-statistic sample dist))
        (test 0.0067167 (z-test sample dist))
        (test 0.0134334 (z-test sample dist 2))
        (test-assert 0.0067167 (z-test? sample dist)))
      (test 9107.30
          (covariance '#(1692 1978 1884 2151 2519) '#(68 102 110 112 154)))
      (let ((a1 '(30.02 29.99 30.11 29.97 30.01 29.99))
            (a2 '(29.89 29.93 29.72 29.98 30.02 29.98)))
        (test 1.959 (t-statistic a1 a2))
        (test 7.030560 (t-df a1 a2))
        (test 0.0453865 (t-test a1 a2))
        (test 0.090773 (t-test a1 a2 2))
        (test-assert (t-test? a1 a2 0.10))
        (test-assert (t-test? a1 a2 0.05))
        (test-not (t-test? a1 a2 0.05 2))
        (test 0.246476
            (t-test a1 (normal-distribution 30.0 0.01))))
      ;; beta is used by the t-test
      (test 0.58031 (beta-pdf 3.5 0.5 0.6479981485767183))
      (test 0.25465 (beta-pdf 3.5 0.5 0.5))
      (test 0.981748 (beta 3.5 0.5))
      (test 0.09216755 (beta 3.5 0.5 0.6479981485767183))
      (test 0.632120558828558 (lower-incomplete-gamma 1 1))
      (test 0.864664716763387 (lower-incomplete-gamma 1 2))
      (test 0.264241117657115 (lower-incomplete-gamma 2 1))
      (test 13.4 (chi^2-statistic '(5 8 9 8 10 20) (make-list 6 10)))
      (test 11.0 (chi^2-statistic '(11 15 8 10 2 14) (make-list 6 10)))
      (test 11.0 (chi^2-statistic '(11 15 8 10 2 14) (die-dist 6)))
      (test 0.0199052 (chi^2-test '(5 8 9 8 10 20) (make-list 6 10)))
      (test 0.051380 (chi^2-test '(11 15 8 10 2 14) (die-dist 6)))
      (test 0.0199052 (chi^2-test 13.4 5))
      (test 0.2301393 (chi^2-test 1.44 1))
      (test-not (chi^2-test? '(11 15 8 10 2 14) (die-dist 6)))
      (test-assert (chi^2-test? '(11 15 8 10 2 14) (die-dist 6) 0.10))
      ;; TODO: verify
      (test 11.7573325 (g-statistic '(5 8 9 8 10 20) (make-list 6 10)))
      (test 0.03826850 (g-test '(5 8 9 8 10 20) (die-dist 6)))
      (test-assert (g-test? '(5 8 9 8 10 20) (die-dist 6)))
      (test 0.026544246 (binomial-test (bernouli-trials 51 235)
                                       (mean-distribution #i1/6)))
      (test 0.04374797 (binomial-test (bernouli-trials 51 235)
                                      (mean-distribution #i1/6)
                                      2))
      (test 0.010635376 (binomial-test (bernouli-trials 13 16)
                                       (mean-distribution 0.5)))
      (test 0.021270752 (binomial-test (bernouli-trials 13 16)
                                       (mean-distribution 0.5)
                                       2))
      (test-assert (binomial-test? (bernouli-trials 51 235)
                                   (mean-distribution #i1/6)
                                   2))
      (test-not (binomial-test? (bernouli-trials 50 235)
                                (mean-distribution #i1/6)
                                2))
      (let ((ages '#(43 21 25 42 57 59))
            (glucose-levels '#(99 65 79 75 87 81)))
        (test .529809
            (pearson-correlation ages glucose-levels)))
      (test '#(7 4 1 5 3 6 2 10 9 8)
          (map-rank '#(106 100 86 101 99 103 97 113 112 110)))
      (test #i-29/165
          (spearman-rank-correlation
           '#(106 100 86 101 99 103 97 113 112 110)
           '#(7 27 2 50 28 29 20 12 6 17)))
      (test 0.6284796155 (kolmogorov-cdf 10 0.274))
      (let ((ls1 '(6 7 9 13 19 21 22 23 24))
            (ls2 '(10 11 12 16 20 27 28 32 44 54)))
        (test 1/2 (kolmogorov-smirnoff-statistic ls1 ls2))
        (test 0.10557709 (kolmogorov-smirnoff-test ls1 ls2))
        (test-not (kolmogorov-smirnoff-test? ls1 ls2))
        (test-assert (kolmogorov-smirnoff-test? ls1 ls1)))
      (let ((ls1 '(6 7 9 13 19 21 22 23 24 29 30 34 36 41 45 47 51 63 33 91))
            (ls2 '(10 11 12 16 20 27 28 32 44 54 56 57 64 69 71 80 81 88 90)))
        (test 0.42631579 (kolmogorov-smirnoff-statistic ls1 ls2))
        (test 0.04629866 (kolmogorov-smirnoff-test ls1 ls2)))
      (let ((ls1 '(-10 -5 17 21 22 23 24 30 44 50 56 57 59 67 73 75 77 78 79
                   80 81 83 84 85 88 90 92 93 94 95 98 100 101 103 105 110))
            (ls2 '(-2 -1 0 10 14 15 16 20 25 26 27 31 32 33 34 45 47 48 51
                   52 53 54 60 61 62 63 74 82 106 107 109 11 112 113 114)))
        (test 0.41031746 (kolmogorov-smirnoff-statistic ls1 ls2))
        (test 0.00300744 (kolmogorov-smirnoff-test ls1 ls2))
        (test 0.00300744 (kolmogorov-smirnoff-test ls2 ls1)))
      (test 0.229792 (kolmogorov-smirnoff-critical 80 62))
      (let ((ls1 '(125 115 130 140 140 115 140 125 140 135))
            (ls2 '(110 122 125 120 140 124 123 137 135 145)))
        (test-values (values 9 9) (wilcoxon-statistic ls1 ls2))
        (test-assert (wilcoxon-test? ls1 ls2)))
      (let ((ls1 '(125 115 130 140 140 115 140 125 140 135 121 139))
            (ls2 '(110 122 125 120 140 124 123 137 135 145 119 138)))
        (test-values (values 14 11) (wilcoxon-statistic ls1 ls2))
        (test 1.959964 (wilcoxon-critical 11 2 0.05))
        (test-not (wilcoxon-test? ls1 ls2)))
      (let ((females '(213.6684 204.4267 184.4746 194.7927))
            (males '(107.2392 148.6768 107.7622 95.1473))
            (kidz '(381.5930 438.9887 359.4772 378.7686)))
        (test 126.5942 (f-statistic (list females males kidz)))
        (test 2.57236e-07 (f-test (list females males kidz))))
      (let ((as '((6 8 4 5 3 4)
                  (8 12 9 11 6 8)
                  (13 9 11 8 7 12))))
        (test 9.5727272 (f-statistic as))
        (test 0.0020926 (f-test as)))
      (test '(2. -1.)
          (call-with-values
              (lambda () (fit-line '(0 1 2 3 4) '(-1. 1. 3. 5. 7.)))
            list))
      (test-end))))
