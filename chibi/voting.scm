
(define (vector-inc! vec index x)
  (vector-set! vec index (+ x (vector-ref vec index))))

(define (assq-inc! ls key count)
  (cond ((assq key ls)
         => (lambda (cell) (set-cdr! cell (+ (cdr cell) count)) ls))
        (else (cons (cons key count) ls))))

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

(define make-tally
  (opt*-lambda (candidates
                (count 0)
                (first-counts (make-vector (vector-length candidates) 0))
                (pairwise #f)
                (distinct #f))
    (%make-tally candidates count first-counts pairwise distinct)))

(define make-paired-tally
  (opt*-lambda (candidates
                (count 0)
                (first-counts (make-vector (vector-length candidates) 0))
                (pairwise (make-vector (square (vector-length candidates)) 0))
                (distinct #f))
    (%make-tally candidates count first-counts pairwise distinct)))

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

(define (votes->tally votes)
  (let ((res (make-tally (extract-candidates votes))))
    (for-each (lambda (x) (tally-inc! res (cdr x))) votes)
    res))

(define (votes->paired-tally votes . o)
  (let* ((candidates (if (pair? o) (car o) (extract-candidates votes)))
         (res (make-paired-tally candidates)))
    (for-each (lambda (vote) (tally-inc! res (cdr vote))) votes)
    res))

(define (votes->distinct-tally votes)
  (let ((res (make-distinct-tally (extract-candidates votes))))
    (for-each (lambda (x) (tally-inc! res (cdr x))) votes)
    res))

(define (tally->distinct-alist tally)
  (hash-table->alist (tally-distinct tally)))

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

(define (sort-pairs x)
  (let ((pairs (if (tally? x) (tally->pairs x) x)))
    (list-sort
     (lambda (a b)
       (or (> (cdr a) (cdr b))
           ;; tie breaker
           (and (= (cdr a) (cdr b))
                (let ((a^-1 (pair-score (cons (cdar a) (caar a)) pairs))
                      (b^-1 (pair-score (cons (cdar b) (caar b)) pairs)))
                  (< a^-1 b^-1)))))
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

(define (tideman-rank x)
  (let ((tally (if (tally? x) x (votes->paired-tally x))))
    (topological-sort (lock-pairs (map car (sort-pairs tally))))))

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
