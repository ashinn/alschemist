
(define-record-type Vote-Tally
  (%make-vote-tally count candidates pairwise)
  vote-tally?
  (count vote-tally-count)
  (candidates vote-tally-candidates)
  (pairwise vote-tally-pairwise))

(define (make-vote-tally count candidates . o)
  (let ((pairwise (if (pair? o)
                      (car o)
                      (make-vector (square (vector-length candidates)) 0))))
    (%make-vote-tally count candidates pairwise)))

;; the number of votes for i over j
(define (vote-tally-ref vote-tally i j)
  (let* ((candidates (vote-tally-candidates vote-tally))
         (i (if (integer? i) i (candidate-index candidates i)))
         (j (if (integer? j) j (candidate-index candidates j))))
    (vector-ref (vote-tally-pairwise vote-tally)
                (+ (* i (vector-length candidates)) j))))

;; increment the number of votes for i over j
(define (vote-tally-inc! vote-tally i j i-count j-count)
  (let* ((candidates (vote-tally-candidates vote-tally))
         (i (if (integer? i) i (candidate-index candidates i)))
         (j (if (integer? j) j (candidate-index candidates j)))
         (i-j (+ (* i (vector-length candidates)) j))
         (j-i (+ (* j (vector-length candidates)) i))
         (vec (vote-tally-pairwise vote-tally)))
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

(define (candidate-index candidates x)
  (or (vector-index (lambda (y) (eq? x y)) candidates)
      (error "unknown candidate" x)))

(define (normalize-vote vote candidates)
  (map (lambda (x)
         (map (lambda (y) (candidate-index candidates y))
              x))
       vote))

(define (tally! vote-tally vote)
  (let* ((candidates (vote-tally-candidates vote-tally))
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
               (lambda (j) (vote-tally-inc! vote-tally j i 1 0))
               used)
              (lp (+ i 1) ls))))))
       (else
        (for-each
         (lambda (j)
           (for-each
            (lambda (i-ls)
              (for-each (lambda (i) (vote-tally-inc! vote-tally j i 1 0))
                        i-ls))
            (cdr ls)))
         (car ls))
        (lp (cdr ls)))))))

(define (tally-votes votes . o)
  (let* ((candidates (if (pair? o) (car o) (extract-candidates votes)))
         (num-candidates (vector-length candidates))
         (vote-tally (make-vote-tally (length votes) candidates)))
    (for-each (lambda (vote) (tally! vote-tally vote)) votes)
    vote-tally))

(define (vote-tally->pairs vote-tally)
  (let* ((candidates (vote-tally-candidates vote-tally))
         (num-candidates (vector-length candidates)))
    (do ((i 0 (+ i 1))
         (ls '()
             (do ((j (+ i 1) (+ j 1))
                  (ls ls
                      `(((,(vector-ref candidates i)
                          . ,(vector-ref candidates j))
                         . ,(vote-tally-ref vote-tally i j))
                        ((,(vector-ref candidates j)
                          . ,(vector-ref candidates i))
                         . ,(vote-tally-ref vote-tally j i))
                        ,@ls)))
                 ((>= j num-candidates) ls))))
        ((= i num-candidates) ls))))

(define (pairs->vote-tally pairs . o)
  ;; the default over-counts
  (let* ((count (if (pair? o)
                    (car o)
                    (fold (lambda (pair sum) (+ (cdr pair) sum)) 0 pairs)))
         (candidates (list->vector
                      (delete-duplicates (append (map caar pairs)
                                                 (map cdar pairs)))))
         (vote-tally (make-vote-tally count candidates)))
    (for-each
     (lambda (pair)
       (vote-tally-inc! vote-tally (caar pair) (cdar pair) (cdr pair) 0))
     pairs)
    vote-tally))

(define (pair-score pair pairs)
  (cond ((assoc pair pairs) => cdr) (else 0)))

(define (sort-pairs x)
  (let ((pairs (if (vote-tally? x) (vote-tally->pairs x) x)))
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

(define (rank-votes x)
  (let ((vote-tally (if (vote-tally? x) x (tally-votes x))))
    (topological-sort (lock-pairs (map car (sort-pairs vote-tally))))))
