
(define-library (chibi voting)
  (import (scheme base) (scheme hash-table) (scheme list)
          (scheme sort) (scheme vector) (scheme write))
  (export Paired-Tally paired-tally? make-paired-tally
          paired-tally-count paired-tally-candidates paired-tally-pairwise
          paired-tally-ref paired-tally-inc!
          tally-votes paired-tally->pairs pairs->paired-tally
          Distinct-Tally distinct-tally? distinct-tally-candidates
          distinct-tally-count distinct-tally-count-set!
          distinct-tally-votes distinct-tally-ref distinct-tally-inc!
          distinct-tally->alist alist->distinct-tally
          sort-pairs lock-pairs
          plurality-rank tideman-rank instant-runoff-rank)
  (include "voting.scm"))
