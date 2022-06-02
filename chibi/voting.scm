
;;> Preferential voting utilities to help come to reasonable decisions
;;> when there are more than 2 options.  Currently we provide 3 common
;;> voting implementations, with batch and incremental tallying.
;;>
;;> In this library, a \scheme{candidate} is a non-numeric object
;;> compared via \scheme{eq?} such as a symbol and a \scheme{tally} is
;;> an object aggregating \var{votes}.  A single \scheme{vote} is a
;;> list of lists of candidates in order of preference.  The inner
;;> lists may have multiple elements to represent an equal preference
;;> among the options, so that the vote \scheme{((A B) (C))}
;;> represents a preference of either \scheme{A} or \scheme{B} equally
;;> over \scheme{C}.  A vote should not have any duplicate candidates,
;;> but need not include all eligible candidates.

;;> \section{Ranking}

;;> Aka "ranked pairs" voting, returns a list of candidates ordered by
;;> the largest pairwise preference above other candidates.  \var{x}
;;> should be either a \var{tally} or alist of votes.  Unlike
;;> \scheme{plurality-rank} and \scheme{instant-runoff-rank} satisfies
;;> the Condorcet winner and loser criteria, the Smith criterion, and
;;> various other criteria desirable in motivating people to vote
;;> honestly and without strategy for their actual preferences.
;;> Recommended for most use cases.
;;>
;;> Note when their are only two or a few voters it is not so uncommon
;;> for a tie to occur when sorting pairs by size of their majority.
;;> For these cases we provide a tie breaker.  We compare the total
;;> number of wins against all other candidates for each winning
;;> candidate in the pairs.
(define (tideman-rank x)
  (let ((tally (if (tally? x) x (votes->paired-tally x))))
    (topological-sort (lock-pairs (map car (sort-pairs tally))))))

;;> Aka "first-past-the-post" voting, returns a list of candidates
;;> ordered by their frequency as the first place vote.  This is the
;;> voting used in most elections.  \var{x}, which can be either a
;;> \var{tally} or alist of votes.  The keys of the alist should be
;;> some identifier of the voter, but are for debugging purposes and
;;> not actually used.  Second or later preferences are allowed but
;;> ignored by the algorithm.  However, unlike traditional plurality
;;> voting, each vote may have multiple first place choice candidates
;;> which are counted equally.  If equal weighted votes are not
;;> desired they should be filtered separately prior to tallying.
(define (plurality-rank x)
  (let* ((tally (if (tally? x) x (votes->tally x)))
         (candidates (tally-candidates tally))
         (first-counts (tally-first-counts tally)))
    (map car
         (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                    (map (lambda (i)
                           (cons (vector-ref candidates i)
                                 (vector-ref first-counts i)))
                         (iota (vector-length candidates)))))))

;;> Aka "IRV" or "ranked-choice voting" (RCV), returns a list of
;;> candidates ordered by successive rounds of removing the last place
;;> candidate.  This is the system used in most of Australia's
;;> elections among other uses, and as such is perhaps the best known
;;> preferential voting system.  However, it fails to satisfy the
;;> Condorcet winner criteria, which is to say that even if there is a
;;> candidate preferred pairwise over all other candidates, they may
;;> not necessarily win.  \var{x} should be either a \var{tally} or
;;> alist of votes.  Unlike traditional IRV this implementation allows
;;> equal weighted votes which should be filtered prior to tallying if
;;> undesired.
(define (instant-runoff-rank x)
  (let* ((tally (if (tally? x) x (votes->distinct-tally x)))
         (ls (tally->distinct-alist tally)))
    (let lp ((ls ls)
             (result '()))
      (if (and (pair? ls)
               (null? (cdr ls))
               (null? (caar ls)))
          result
          (let ((counts (first-pref-counts ls)))
            (if (null? counts)
                result
                (let ((candidate (min-candidate counts)))
                  (lp (remove-candidate ls candidate)
                      (cons candidate result)))))))))

;;> \section{Tallying}

;;> A \scheme{Tally} is an object holding an aggregated count of
;;> preferential votes.  On creation you specify what level of
;;> aggregation is done.  The most general case is \scheme{distinct},
;;> which keeps a count of each unique preferential vote.  The next
;;> most general is \scheme{pairwise}, which counts just pairwise
;;> preferences, followed by the default which just counts the number
;;> of times a candidate appeared as the first choice.
(define-record-type Tally
  (%make-tally candidates count first-counts pairwise distinct)
  tally?
  (candidates tally-candidates)
  (count tally-count tally-count-set!)
  (first-counts tally-first-counts)
  (pairwise tally-pairwise)
  (distinct tally-distinct))

(define (candidate-index candidates x)
  (if (integer? x)
      (if (or (inexact? x) (negative? x))
          (error "candidate should be a symbol or natural number" x)
          x)
      (or (vector-index (lambda (y) (eq? x y))
                        (if (tally? candidates)
                            (tally-candidates candidates)
                            candidates))
          (error "unknown candidate" x))))

(define (normalize-vote vote candidates)
  (let ((candidates (if (tally? candidates)
                        (tally-candidates candidates)
                        candidates)))
    (map (lambda (x)
           (map (lambda (y) (candidate-index candidates y))
                x))
         vote)))

;;> Returns a tally of candidate first place counts.  \var{candidates}
;;> should be a vector of all valid candidates.
(define make-tally
  (opt*-lambda (candidates
                (count 0)
                (first-counts (make-vector (vector-length candidates) 0))
                (pairwise #f)
                (distinct #f))
    (%make-tally candidates count first-counts pairwise distinct)))

;;> Returns a tally of candidate vs candidate pairwise counts.
(define make-paired-tally
  (opt*-lambda (candidates
                (count 0)
                (first-counts (make-vector (vector-length candidates) 0))
                (pairwise (make-vector (square (vector-length candidates)) 0))
                (distinct #f))
    (%make-tally candidates count first-counts pairwise distinct)))

;;> Returns a tally of counts of each distinct preferential vote, in
;;> addition to pairwise and first place counts.
(define make-distinct-tally
  (opt*-lambda (candidates
                (count 0)
                (first-counts (make-vector (vector-length candidates) 0))
                (pairwise (make-vector (square (vector-length candidates)) 0))
                (distinct (make-hash-table equal?)))
    (%make-tally candidates count first-counts pairwise distinct)))

(define (tally-num-candidates tally)
  (vector-length (tally-candidates tally)))

(define (tally-distinct-ref tally vote)
  (hash-table-ref/default (tally-distinct tally) vote 0))

(define (tally-pairwise-index tally i j)
  (+ (* (candidate-index (tally-candidates tally) i)
        (tally-num-candidates tally))
     (candidate-index (tally-candidates tally) j)))

(define (tally-pairwise-ref tally i j)
  (vector-ref (tally-pairwise tally)
              (tally-pairwise-index tally i j)))

(define (vector-inc! vec index x)
  (vector-set! vec index (+ x (vector-ref vec index))))

(define (tally-inc-pairwise! tally vote . o)
  (let* ((count (if (pair? o) (car o) 1))
         (candidates (tally-candidates tally))
         (vote (normalize-vote vote candidates))
         (used (list-sort < (concatenate vote)))
         (num-candidates (vector-length candidates))
         (vec (tally-pairwise tally)))
    (let lp ((ls vote))
      (cond
       ((null? ls)
        (let lp ((i 0) (ls used))
          (when (< i num-candidates)
            (cond
             ((and (pair? ls) (= i (car ls)))
              ;; i was used, keep going
              (lp (+ i 1) (cdr ls)))
             (else
              ;; record a vote of each used candidate over i
              (for-each
               (lambda (j)
                 (vector-inc! vec (tally-pairwise-index tally j i) count))
               used)
              (lp (+ i 1) ls))))))
       (else
        (for-each
         (lambda (j)
           (for-each
            (lambda (i-ls)
              (for-each
               (lambda (i)
                 (vector-inc! vec (tally-pairwise-index tally j i) count))
               i-ls))
            (cdr ls)))
         (car ls))
        (lp (cdr ls)))))))

;;> Add a new \var{vote} to the \var{tally}.
(define (tally-inc! tally vote . o)
  (let ((count (if (pair? o) (car o) 1)))
    ;; always update count and first-counts
    (tally-count-set! tally (+ (tally-count tally) count))
    (let ((first-counts (tally-first-counts tally)))
      (for-each
       (lambda (x)
         (let ((i (if (integer? x)
                      x
                      (candidate-index
                       (tally-candidates tally) x))))
           (vector-set! first-counts i (+ count (vector-ref first-counts i)))))
       (car vote)))
    ;; update distinct and pairwise if present
    (when (tally-distinct tally)
      (hash-table-update!/default
       (tally-distinct tally)
       vote
       (lambda (x) (+ x count))
       0))
    (when (tally-pairwise tally)
      (tally-inc-pairwise! tally vote count))))

;;> Returns a \scheme{tally} for the votes alist \var{votes}.
(define (votes->tally votes)
  (let ((res (make-tally (extract-candidates votes))))
    (for-each (lambda (x) (tally-inc! res (cdr x))) votes)
    res))

;;> Returns a \scheme{tally} with pairwise aggregation for the votes
;;> alist \var{votes}.
(define (votes->paired-tally votes . o)
  (let* ((candidates (if (pair? o) (car o) (extract-candidates votes)))
         (res (make-paired-tally candidates)))
    (for-each (lambda (vote) (tally-inc! res (cdr vote))) votes)
    res))

;;> Returns a \scheme{tally} with distinct aggregation for the votes
;;> alist \var{votes}.
(define (votes->distinct-tally votes)
  (let ((res (make-distinct-tally (extract-candidates votes))))
    (for-each (lambda (x) (tally-inc! res (cdr x))) votes)
    res))

;;> Returns an alist of distinct vote to count for the given \var{tally}.
(define (tally->distinct-alist tally)
  (hash-table->alist (tally-distinct tally)))

;;> Returns a new tally with distinct aggregation for the given vote
;;> to count alist.
(define (distinct-alist->tally ls)
  (let ((res (make-distinct-tally
              (list->vector
               (fold (lambda (x res)
                       (delete-duplicates (append (concatenate (car x)) res)))
                     '()
                     ls)))))
    (for-each (lambda (x) (tally-inc! res (car x) (cdr x))) ls)
    res))

(define (union/eq a b)
  (cond ((null? a) b)
        ((memq (car a) b) (union/eq (cdr a) b))
        (else (union/eq (cdr a) (cons (car a) b)))))

(define (join-candidates ranked res)
  (let lp ((ls ranked) (res res))
    (cond ((null? ls) res)
          ((symbol? (car ls)) (lp (cdr ls) (union/eq (list (car ls)) res)))
          (else (lp (cdr ls) (union/eq (car ls) res))))))

(define (extract-candidates votes)
  (let lp ((ls votes) (res '()))
    (if (null? ls)
        (list->vector (reverse res))
        (lp (cdr ls) (join-candidates (cdar ls) res)))))

;;> Returns an alist with \scheme{(candidate . candidate)} pair keys
;;> and count values for the given \var{tally}.
(define (tally->pairs tally)
  (let* ((candidates (tally-candidates tally))
         (num-candidates (vector-length candidates)))
    (do ((i 0 (+ i 1))
         (ls '()
             (do ((j (+ i 1) (+ j 1))
                  (ls ls
                      `(((,(vector-ref candidates i)
                          . ,(vector-ref candidates j))
                         . ,(tally-pairwise-ref tally i j))
                        ((,(vector-ref candidates j)
                          . ,(vector-ref candidates i))
                         . ,(tally-pairwise-ref tally j i))
                        ,@ls)))
                 ((>= j num-candidates) ls))))
        ((= i num-candidates) ls))))

;;> Returns a new \scheme{tally} with pairwise aggregation for the
;;> given alist of pairs having \scheme{(candidate . candidate)} keys
;;> and count values.
(define (pairs->tally pairs . o)
  (let* ((count (if (pair? o)
                    (car o)
                    (fold (lambda (pair sum) (+ (cdr pair) sum)) 0 pairs)))
         (candidates (list->vector
                      (delete-duplicates (append (map caar pairs)
                                                 (map cdar pairs)))))
         (num-candidates (vector-length candidates))
         (tally (make-paired-tally candidates count))
         (pairwise (tally-pairwise tally)))
    (for-each
     (lambda (pair)
       (let ((index (tally-pairwise-index tally (caar pair) (cdar pair))))
         (vector-inc! pairwise index (cdr pair))))
     pairs)
    tally))

(define (pair-score pair pairs)
  (cond ((assoc pair pairs) => cdr) (else 0)))

(define (candidate-pairwise-wins a pairs)
  (let* ((left (filter (lambda (x) (eq? a (caar x))) pairs))
         (right (filter (lambda (x) (eq? a (cdar x))) pairs))
         (total1 (fold + 0 (map cdr left)))
         (total2 (fold + 0 (map cdr right)))
         (avg (/ (+ total1 total2) (+ (length left) (length right)))))
    (count (lambda (x) (and (eq? a (caar x)) (> (cdr x) avg))) pairs)))

(define (sort-pairs x)
  (let ((pairs (if (tally? x) (tally->pairs x) x)))
    (list-sort
     (lambda (a b)
       (or (> (cdr a) (cdr b))
           (and (= (cdr a) (cdr b))
                ;; tie breaker 1: check the total number of wins
                (let ((a-wins (candidate-pairwise-wins (caar a) pairs))
                      (b-wins (candidate-pairwise-wins (caar b) pairs)))
                  (or (> a-wins b-wins)
                      (and (= a-wins b-wins)
                           ;; tie breaker 2: check if the score for
                           ;; the inverse of the pair is smaller
                           (let ((a^-1 (pair-score (cons (cdar a) (caar a))
                                                   pairs))
                                 (b^-1 (pair-score (cons (cdar b) (caar b))
                                                   pairs)))
                             (< a^-1 b^-1))))))))
     pairs)))

(define (insert-edge a b graph)
  (let lp ((ls graph) (rev '()))
    (cond
     ((null? ls)
      (cons (list a b) graph))
     ((equal? a (caar ls))
      (if (member b (cdar ls))
          graph
          (append (reverse rev)
                  (cons (cons (caar ls) (cons b (cdar ls))) (cdr ls)))))
     (else
      (lp (cdr ls) (cons (car ls) rev))))))

(define (graph-ref graph a)
  (cond ((assoc a graph) => cdr) (else '())))

;; can a be reached from b with the given graph?
(define (graph-reachable? a b graph)
  (let lp ((ls (graph-ref graph b))
           (seen '()))
    (cond
     ((null? ls) #f)
     ((equal? a (car ls)) #t)
     (else
      (lp (append (remove (lambda (x) (member x seen))
                          (graph-ref graph (car ls)))
                  (cdr ls))
          (cons (car ls) seen))))))

(define (lock-pairs pairs)
  (let lp ((ls pairs) (graph '()))
    (cond
     ((null? ls)
      graph)
     ((graph-reachable? (caar ls) (cdar ls) graph)
      (lp (cdr ls) graph))
     (else
      (lp (cdr ls) (insert-edge (caar ls) (cdar ls) graph))))))

(define (topological-sort graph)
  (let visit ((ls graph) (seen '()) (res '()) (return (lambda (seen res) res)))
    (cond
     ((null? ls)
      (return seen res))
     ((member (car (car ls)) seen)
      (visit (cdr ls) seen res return))
     ((member (car (car ls)) res)
      (visit (cdr ls) seen res return))
     (else
      (let scan-deps ((deps (cdr (car ls)))
                      (seen (cons (car (car ls)) seen))
                      (res res))
        (cond
         ((null? deps)
          (visit (cdr ls) seen (cons (car (car ls)) res) return))
         ((member (car deps) seen)
          (scan-deps (cdr deps) seen res))
         ((member (car deps) res)
          (scan-deps (cdr deps) seen res))
         ((assoc (car deps) graph)
          => (lambda (vertices)
               (visit (list vertices)
                      seen
                      res
                      (lambda (seen res)
                        (scan-deps (cdr deps) seen res)))))
         (else
          (scan-deps (cdr deps) seen (cons (car deps) res)))))))))

(define (assq-inc! ls key count)
  (cond ((assq key ls)
         => (lambda (cell) (set-cdr! cell (+ (cdr cell) count)) ls))
        (else (cons (cons key count) ls))))

(define (first-pref-counts ls)
  (let lp ((ls ls) (counts '()))
    (if (null? ls)
        counts
        (lp (cdr ls)
            (fold
             (lambda (candidate counts)
               (assq-inc! counts candidate (cdar ls)))
             counts
             (car (caar ls)))))))

(define (min-candidate counts)
  (let lp ((ls (cdr counts))
           (min-cand (caar counts))
           (min-count (cdar counts)))
    (cond
     ((null? ls)
      min-cand)
     ((< (cdar ls) min-count)
      (lp (cdr ls) (caar ls) (cdar ls)))
     (else
      (lp (cdr ls) min-cand min-count)))))

(define (remove-candidate ls candidate)
  (let ((counts (make-hash-table equal?)))
    (for-each
     (lambda (x)
       (let ((vote (remove null?
                           (map (lambda (y)
                                  (delete candidate y))
                                (car x)))))
         (when (pair? vote)
           (hash-table-update!/default counts vote (lambda (y) (+ y (cdr x))) 0))))
     ls)
    (hash-table->alist counts)))
