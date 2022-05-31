
(define (candidate-index candidates x)
  (or (vector-index (lambda (y) (eq? x y)) candidates)
      (error "unknown candidate" x)))

;; A tally of the number of times each distinct vote is seen, useful
;; for systems such as instant runoff.
(define-record-type Distinct-Tally
  (%make-distinct-tally candidates count votes first-count)
  distinct-tally?
  (candidates distinct-tally-candidates)
  (count distinct-tally-count distinct-tally-count-set!)
  (votes distinct-tally-votes)
  (first-count distinct-tally-first-count))

(define (make-distinct-tally candidates . o)
  (let ((count (if (pair? o) (car o) 0))
        (votes (if (and (pair? o) (pair? (cdr o)))
                   (cadr o)
                   (make-hash-table equal?)))
        (first-count (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                         (car (cddr o))
                         (make-vector (vector-length candidates) 0))))
    (%make-distinct-tally candidates count votes first-count)))

(define (distinct-tally-ref distinct-tally vote)
  (hash-table-ref/default (distinct-tally-votes distinct-tally) vote 0))

(define (distinct-tally-inc! distinct-tally vote . o)
  (let ((count (if (pair? o) (car o) 1)))
    (distinct-tally-count-set! distinct-tally
                               (+ (distinct-tally-count distinct-tally)
                                  count))
    (let ((first-count (distinct-tally-first-count distinct-tally)))
      (for-each
       (lambda (x)
         (let ((i (if (integer? x)
                      x
                      (candidate-index
                       (distinct-tally-candidates distinct-tally) x))))
           (vector-set! first-count i (+ count (vector-ref first-count i)))))
       (car vote)))
    (hash-table-update!/default
     (distinct-tally-votes distinct-tally)
     vote
     (lambda (x) (+ x count))
     0)))

(define (distinct-tally->alist distinct-tally)
  (hash-table->alist (distinct-tally-votes distinct-tally)))

(define (alist->distinct-tally ls)
  (let ((res (make-distinct-tally
              (list->vector
               (fold (lambda (x res)
                       (delete-duplicates (append (concatenate (car x)) res)))
                     '()
                     ls)))))
    (for-each (lambda (x) (distinct-tally-inc! res (car x) (cdr x))) ls)
    res))

;; A tally of how each candidate ranks over each other, useful for
;; systems such as Tideman's (i.e. ranked pairs).
(define-record-type Paired-Tally
  (%make-paired-tally candidates count pairwise)
  paired-tally?
  (candidates paired-tally-candidates)
  (count paired-tally-count paired-tally-count-set!)
  (pairwise paired-tally-pairwise))

(define (make-paired-tally candidates . o)
  (let ((count (if (pair? o) (car o) 0))
        (pairwise (if (and (pair? o) (pair? (cdr o)))
                      (cadr o)
                      (make-vector (square (vector-length candidates)) 0))))
    (%make-paired-tally candidates count pairwise)))

;; the number of votes for i over j
(define (paired-tally-ref paired-tally i j)
  (let* ((candidates (paired-tally-candidates paired-tally))
         (i (if (integer? i) i (candidate-index candidates i)))
         (j (if (integer? j) j (candidate-index candidates j))))
    (vector-ref (paired-tally-pairwise paired-tally)
                (+ (* i (vector-length candidates)) j))))

;; increment the number of votes for i over j
(define (paired-tally-inc! paired-tally i j i-count j-count)
  (let* ((candidates (paired-tally-candidates paired-tally))
         (i (if (integer? i) i (candidate-index candidates i)))
         (j (if (integer? j) j (candidate-index candidates j)))
         (i-j (+ (* i (vector-length candidates)) j))
         (j-i (+ (* j (vector-length candidates)) i))
         (vec (paired-tally-pairwise paired-tally)))
    (paired-tally-count-set! paired-tally
                             (+ (paired-tally-count paired-tally)
                                i-count j-count))
    (vector-set! vec i-j (+ i-count (vector-ref vec i-j)))
    (vector-set! vec j-i (+ j-count (vector-ref vec j-i)))))

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

(define (normalize-vote vote candidates)
  (map (lambda (x)
         (map (lambda (y) (candidate-index candidates y))
              x))
       vote))

(define (tally! paired-tally vote)
  (let* ((candidates (paired-tally-candidates paired-tally))
         (vote (normalize-vote (cdr vote) candidates))
         (used (list-sort < (concatenate vote)))
         (num-candidates (vector-length candidates)))
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
               (lambda (j) (paired-tally-inc! paired-tally j i 1 0))
               used)
              (lp (+ i 1) ls))))))
       (else
        (for-each
         (lambda (j)
           (for-each
            (lambda (i-ls)
              (for-each (lambda (i) (paired-tally-inc! paired-tally j i 1 0))
                        i-ls))
            (cdr ls)))
         (car ls))
        (lp (cdr ls)))))))

(define (tally-votes votes . o)
  (let* ((candidates (if (pair? o) (car o) (extract-candidates votes)))
         (num-candidates (vector-length candidates))
         (paired-tally (make-paired-tally candidates (length votes))))
    (for-each (lambda (vote) (tally! paired-tally vote)) votes)
    paired-tally))

(define (paired-tally->pairs paired-tally)
  (let* ((candidates (paired-tally-candidates paired-tally))
         (num-candidates (vector-length candidates)))
    (do ((i 0 (+ i 1))
         (ls '()
             (do ((j (+ i 1) (+ j 1))
                  (ls ls
                      `(((,(vector-ref candidates i)
                          . ,(vector-ref candidates j))
                         . ,(paired-tally-ref paired-tally i j))
                        ((,(vector-ref candidates j)
                          . ,(vector-ref candidates i))
                         . ,(paired-tally-ref paired-tally j i))
                        ,@ls)))
                 ((>= j num-candidates) ls))))
        ((= i num-candidates) ls))))

(define (pairs->paired-tally pairs . o)
  ;; the default over-counts
  (let* ((count (if (pair? o)
                    (car o)
                    (fold (lambda (pair sum) (+ (cdr pair) sum)) 0 pairs)))
         (candidates (list->vector
                      (delete-duplicates (append (map caar pairs)
                                                 (map cdar pairs)))))
         (paired-tally (make-paired-tally candidates count)))
    (for-each
     (lambda (pair)
       (paired-tally-inc! paired-tally (caar pair) (cdar pair) (cdr pair) 0))
     pairs)
    paired-tally))

(define (pair-score pair pairs)
  (cond ((assoc pair pairs) => cdr) (else 0)))

(define (sort-pairs x)
  (let ((pairs (if (paired-tally? x) (paired-tally->pairs x) x)))
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
  (let ((paired-tally (if (paired-tally? x) x (tally-votes x))))
    (topological-sort (lock-pairs (map car (sort-pairs paired-tally))))))

(define (votes->distinct-tally votes)
  (let ((res (make-distinct-tally (extract-candidates votes))))
    (for-each (lambda (x) (distinct-tally-inc! res (cdr x))) votes)
    res))

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

(define (instant-runoff-rank x)
  (let* ((distinct-tally (if (distinct-tally? x) x (votes->distinct-tally x)))
         (ls (distinct-tally->alist distinct-tally)))
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
  (let* ((distinct-tally (if (distinct-tally? x) x (votes->distinct-tally x)))
         (candidates (distinct-tally-candidates distinct-tally))
         (first-count (distinct-tally-first-count distinct-tally)))
    (map car
         (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                    (map (lambda (i)
                           (cons (vector-ref candidates i)
                                 (vector-ref first-count i)))
                         (iota (vector-length candidates)))))))
