
(define-library (chibi voting)
  (import (scheme base) (scheme hash-table) (scheme list)
          (scheme sort) (scheme vector) (srfi 227))
  (export Tally tally? make-tally make-paired-tally make-distinct-tally
          tally-inc!
          tally-distinct-ref tally-pairwise-ref
          tally-candidates tally-count tally-count-set! tally-distinct
          tally-first-counts tally-pairwise
          tally->distinct-alist distinct-alist->tally
          tally->pairs pairs->tally votes->paired-tally
          sort-pairs lock-pairs
          plurality-rank tideman-rank instant-runoff-rank)
  (include "voting.scm"))
