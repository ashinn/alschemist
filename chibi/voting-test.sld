
(define-library (chibi voting-test)
  (import (scheme base) (chibi voting) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "(chibi voting)")
      ;; tally then rank from indidivual votes
      (let ((votes '((alice (A) (B) (C) (D))
                     (bob (B) (C) (D) (A))
                     (charlie (C) (A) (B) (D))
                     (dave (D) (A) (B) (C))
                     )))
        (test '(((C . D) . 3) ((B . D) . 3) ((B . C) . 3) ((A . B) . 3)
                ((A . D) . 2) ((D . A) . 2) ((A . C) . 2) ((C . A) . 2)
                ((D . C) . 1) ((D . B) . 1) ((C . B) . 1) ((B . A) . 1))
            (sort-pairs (tally-votes votes)))
        (test  '(A B C D) (tideman-rank votes))
        (test  '(A C D B) (instant-runoff-rank votes)))
      ;; rank from pre-tallied pairwise outcomes
      (let ((vote-tally
             (pairs->paired-tally
              '(((memphis . nashville) . 42)
                ((memphis . chattanooga) . 42)
                ((memphis . knoxville) . 42)
                ((nashville . memphis) . 58)
                ((nashville . chattanooga) . 68)
                ((nashville . knoxville) . 68)
                ((chattanooga . memphis) . 58)
                ((chattanooga . nashville) . 32)
                ((chattanooga . knoxville) . 83)
                ((knoxville . memphis) . 58)
                ((knoxville . nashville) . 32)
                ((knoxville . chattanooga) . 17)))))
        (test '(nashville chattanooga knoxville memphis)
            (tideman-rank vote-tally)))
      ;; IRV rank from distinct
      (let ((distinct-tally
             (alist->distinct-tally
              '((((A) (B)) . 36)
                (((B) (A)) . 10)
                (((B) (C)) . 20)
                (((C) (B)) . 34)))))
        ;; B is eliminated first with only 30 votes then C beats A
        ;; with 54 to 46 votes
        (test '(C A B)
            (instant-runoff-rank distinct-tally))
        ;; Using only first choices gives a different result.
        (test '(A C B)
            (plurality-rank distinct-tally)))
      (test-end))))
