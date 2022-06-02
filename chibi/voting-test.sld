
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
        (test '(((B . D) . 3) ((B . C) . 3) ((C . D) . 3) ((A . B) . 3)
                ((A . D) . 2) ((A . C) . 2) ((C . A) . 2) ((D . A) . 2)
                ((B . A) . 1) ((C . B) . 1) ((D . C) . 1) ((D . B) . 1))
            (sort-pairs (votes->paired-tally votes)))
        (test  '(A B C D) (tideman-rank votes))
        ;; Since initially each candidate is first pref of one voter,
        ;; the first candidate eliminated (and thus overall result) is
        ;; actually random.
        (test  '(A C B D) (instant-runoff-rank votes)))
      (let ((votes '((eva-lu-ator
                      (ben-bitdiddle)
                      (alyssa-p-hacker)
                      (louis-reasoner)
                      (lem-e-tweakit)
                      (gerald-sussman)
                      (guy-steele))
                     (cy-d-fect
                      (alyssa-p-hacker)
                      (louis-reasoner)
                      (ben-bitdiddle)
                      (lem-e-tweakit)
                      (gerald-sussman)
                      (guy-steele)))))
        (test '(alyssa-p-hacker ben-bitdiddle louis-reasoner
                lem-e-tweakit gerald-sussman guy-steele)
            (tideman-rank votes))
        (test '(alyssa-p-hacker ben-bitdiddle louis-reasoner
                lem-e-tweakit gerald-sussman guy-steele)
            (instant-runoff-rank votes)))
      ;; rank from pre-tallied pairwise outcomes
      (let ((tally
             (pairs->tally
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
            (tideman-rank tally)))
      ;; Comparison of methods.
      (let ((tally
             (distinct-alist->tally
              '(((("A") ("B")) . 36)
                ((("B") ("A")) . 10)
                ((("B") ("C")) . 20)
                ((("C") ("B")) . 34)))))
        ;; Using only first choices gives preference to A.
        (test '("A" "C" "B")
            (plurality-rank tally))
        ;; With IRV, B is eliminated first with only 30 votes,
        ;; then C beats A with 54 to 46 votes.
        (test '("C" "A" "B")
            (instant-runoff-rank tally))
        ;; Ranked pairs prefers B which is never in 3rd place.
        (test '("B" "C" "A")
            (tideman-rank tally)))
      (test-end))))
