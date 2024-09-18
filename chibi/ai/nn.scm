
(define learning-rate (make-parameter 0.01))
(define max-learning-iterations (make-parameter 10000000))
(define learning-batch-size (make-parameter 1000))
(define loss-epsilon (make-parameter 0.0))
(define velocity-retained (make-parameter .9))
(define gradient-decay-rate (make-parameter .9))
(define gradient-stabilizer (make-parameter 1e-8))

;; There are various differences but tentatively using the signature
;; of The Little Learner, allowing us to orthogonally control aspects
;; of gradient descent such as stochastic or velocity.

(define (gradient-descent inflate deflate update)
  (lambda (obj-fn weights)
    (let lp ((weights (map inflate (map as-dual weights)))
             (revs 0))
      (let ((loss (obj-fn (map deflate weights))))
        (if (or (<= (abs (dual-value loss)) (loss-epsilon))
                (>= revs (max-learning-iterations)))
            (map deflate weights)
            (lp (map update
                     weights
                     (gradient loss (map deflate weights)))
                (+ revs 1)))))))

;; Note at least for now we don't do full rank polymorphism,
;; preferring to keep signatures simple, so the target should take a
;; single row, not a batch.

(define (l2-loss target)
  (lambda (xs ys)
    (lambda (weights)
      (.mean
       (array-map (lambda (x y)
                    (let ((pred-y ((target (const x)) weights)))
                      (.square (.- (const y) pred-y))))
                  (array-rows xs)
                  ys)))))

(define (sampling-loss expectant xs ys . o)
  (let* ((random-source (if (pair? o) (car o) default-random-source))
         (n (interval-width (array-domain xs) 0))
         (dist (discrete-uniform-distribution 0 (- n 1) 1 random-source)))
    (lambda (weights)
      (let ((indices (random-multi-sample (learning-batch-size) dist)))
        ((expectant (array-select/copy xs 0 indices)
                    (array-select/copy ys 0 indices))
         weights)))))

(define (naked-i w) w)
(define (naked-d w) w)
(define (naked-u w g)
  ;; It's OK to keep the duals here but faster to unwrap them.
  (.- (dual-value w) (.* (learning-rate) (dual-value g))))

(define naked-gradient-descent
  (gradient-descent naked-i naked-d naked-u))

(define (velocity-i w)
  (list w (if (array? (dual-value w)) (zeros (dual-value w)) 0.)))
(define (velocity-d w)
  (car w))
(define (velocity-u w g)
  (let ((v (.- (.* (velocity-retained) (dual-value (cadr w)))
               (.* (learning-rate) (dual-value g)))))
    (list (as-dual (.+ (dual-value (car w)) v))
          (dual-value v))))

(define velocity-gradient-descent
  (gradient-descent velocity-i velocity-d velocity-u))

(define (smooth decay-rate average x)
  (array+ (array* (dual-value decay-rate) (dual-value average))
          (array* (array- 1. (dual-value decay-rate)) (dual-value x))))

(define (rms-i w)
  (list w (if (array? (dual-value w)) (zeros (dual-value w)) 0.)))
(define (rms-d w)
  (car w))
(define (rms-u w g)
  (let* ((r (smooth (gradient-decay-rate)
                    (cadr w)
                    (array-square (dual-value g))))
         (a (array/ (learning-rate)
                    (array+ (array-sqrt r) (gradient-stabilizer)))))
    (list (as-dual (array- (dual-value (car w))
                           (array* a (dual-value g))))
          r)))

(define rms-gradient-descent
  (gradient-descent rms-i rms-d rms-u))

(define (adam-i w)
  (let ((z (if (array? (dual-value w)) (zeros (dual-value w)) 0.)))
    (list w z z)))
(define (adam-d w)
  (car w))
(define (adam-u w g)
  (let* ((r (smooth (gradient-decay-rate)
                    (car (cddr w))
                    (array-square (dual-value g))))
         (a (array/ (learning-rate)
                    (array+ (array-sqrt r) (gradient-stabilizer))))
         (v (smooth (velocity-retained) (cadr w) g)))
    (list (as-dual (array- (dual-value (car w))
                           (array* a (dual-value g))))
          v
          r)))

(define adam-gradient-descent
  (gradient-descent adam-i adam-d adam-u))

(define-record-type Block
  (make-block fn shape-list name)
  block?
  (fn block-fn)
  (shape-list block-ls)
  (name block-name))

(define (block fn shape-list . o)
  (make-block fn shape-list (and (pair? o) (car o))))

(define (vector->matrix v)
  (specialized-array-reshape
   v
   (make-interval (vector (interval-width (array-domain v) 0) 1))))

(define (linear t)
  (lambda (weights)
    (.+ (.sum-axis/squeeze (.@ (first weights) (vector->matrix t)))
        (second weights))))

(define (relu t)
  (lambda (weights)
    (.rectify ((linear t) weights))))

(define (block-compose f g j)
  (lambda (t)
    (lambda (weights)
      ((g ((f t) weights))
       (drop weights j)))))

(define (stack2 ba bb)
  (block
   (block-compose
    (block-fn ba)
    (block-fn bb)
    (length (block-ls ba)))
   (append
    (block-ls ba)
    (block-ls bb))))

(define (flip f) (lambda (a b) (f b a)))

(define (stack-blocks blocks)
  (fold (flip stack2) (car blocks) (cdr blocks)))

(define (dense-block n m)
  (block relu
         (list (list m n)
               (list m))))

(define (init-weights shapes)
  (map init-shape shapes))

(define (init-shape shape)
  (if (= 1 (length shape))
      (zeros (make-interval (vector (first shape))))
      (random-array 0. (/ 2. (second shape)) shape)))

(define (random-array mean variance shape . o)
  (let ((storage (if (pair? o) (car o) generic-storage-class)))
    (specialized-array-reshape
     (make-specialized-array-from-data
      (random-sample (fold * 1 shape) (normal-distribution mean variance))
      storage)
     (make-interval (list->vector shape)))))

(define (model target weights)
  (lambda (t)
    ((target t) weights)))
