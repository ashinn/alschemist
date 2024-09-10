
(define learning-rate (make-parameter 0.01))
(define max-learning-iterations (make-parameter 10000000))
(define learning-batch-size (make-parameter 1000))
(define loss-epsilon (make-parameter 0.0))
(define velocity-retained (make-parameter .9))

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
                    (let ((pred-y ((target x) weights)))
                      (.square (.- y pred-y))))
                  (array-rows xs)
                  ys)))))

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
