
(define learning-rate (make-parameter 0.01))
(define max-learning-iterations (make-parameter 10000000))
(define learning-batch-size (make-parameter 1000))
(define loss-epsilon (make-parameter 0.0))
(define velocity-retained (make-parameter .9))
(define gradient-decay-rate (make-parameter .9))
(define gradient-stabilizer (make-parameter 1e-8))

;; If we have a 1-dimensional array, we can assume that each element
;; is a scalar row and not bother artificially wrapping the elements
;; in a 0-dimensional array.
;; (define (flat-array-rows a)
;;   (if (= 1 (array-dimension a))
;;       a
;;       (array-rows a)))

(define (l2-loss target)
  (lambda (xs ys)
    (lambda (weights)
      ;; (array-map (lambda (x y)
      ;;              (let ((pred-y ((target (const x)) weights)))
      ;;                ;;(write `(pred ,(interval-widths (array-domain x)) => ,(interval-widths (array-domain y)) actual: ,(interval-widths (array-domain (dual-value pred-y))))) (newline)
      ;;                (.square (.- (const y) pred-y))))
      ;;            (flat-array-rows xs)
      ;;            (flat-array-rows ys))
      (let ((pred-ys ((target (const xs)) weights)))
        (dbg "pred-ys: " pred-ys)
        (.mean (.sum-axis (.square (.- pred-ys ys))))))))

(define (sampling-loss expectant xs ys . o)
  (let* ((random-source (if (pair? o) (car o) default-random-source))
         (n (interval-width (array-domain xs) 0))
         (dist (discrete-uniform-distribution 0 (- n 1) 1 random-source)))
    (lambda (weights)
      (let* ((indices (random-multi-sample (learning-batch-size) dist))
             (sample-xs (array-select/copy xs 0 indices))
             (sample-ys (array-select/copy ys 0 indices)))
        (dbg "sample-xs: " sample-xs " sample-ys: " sample-ys)
        ((expectant sample-xs sample-ys)
         weights)))))

;; There are various differences but tentatively using the signature
;; of The Little Learner, allowing us to orthogonally control aspects
;; of gradient descent such as stochastic or velocity.

(define (gradient-descent inflate deflate update)
  ;; Ensure the weights are duals and all have a label for debugging.
  (define (label-weights weights)
    (let lp ((i 0) (ls weights) (res '()))
      (cond
       ((null? ls) (reverse res))
       ;;((dual-label (car ls)) (lp (+ i 1) (cdr ls) (cons (car ls) res)))
       (else
        (let ((label (string->symbol (string-append "w" (number->string i)))))
          (lp (+ i 1) (cdr ls) (cons (dual-with-label (car ls) label) res)))))))
  ;; (define (preserve-scalar-weights update)
  ;;   (lambda (weight gradient)
  ;;     (let ((new-weight (update weight gradient)))
  ;;       (if (and (number? (dual-value weight))
  ;;                (array? (dual-value new-weight))
  ;;                (= 1 (interval-volume (array-domain (dual-value new-weight)))))
  ;;           (begin
  ;;             (write `(unwrapping ,(array-dimension (dual-value new-weight)) : ,(array->list* (dual-value new-weight)) => ,(array-first (dual-value new-weight)))) (newline)
  ;;             (array-first (dual-value new-weight)))
  ;;           new-weight))))
  (lambda (obj-fn weights)
    (let lp ((weights (map inflate (label-weights (map as-dual weights))))
             (revs 0))
      (let ((deflated-weights (label-weights (map deflate weights))))
        (if (>= revs (max-learning-iterations))
            deflated-weights
            (let ((loss (obj-fn deflated-weights)))
              (info "gradient-descent " (number->string revs) " "
                    (map dual-label deflated-weights))
              (info "loss: " (dual-value loss))
              (if (<= (abs (dual-value loss)) (loss-epsilon))
                  deflated-weights
                  (lp (map update
                           weights
                           (gradient loss deflated-weights))
                      (+ revs 1)))))))))

(define (naked-i w) w)
(define (naked-d w) w)
(define (naked-u w g)
  ;; It's OK to keep the duals here but faster to unwrap them.
  ;;(.- w (.* (learning-rate) g))
  (let ((res (.- (dual-value w) (.* (learning-rate) (dual-value g)))))
    (dbg "update: " w " => " `(- ,w (* ,(learning-rate) ,g)) " => " res)
    res))

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

(define (linear t)
  ;; (write `(linear ,(dual-value t))) (newline)
  (lambda (weights)
    ;; (.+ (.sum-axis/squeeze (.@ (first weights) (.transpose t)))
    ;;     (second weights))
    (.+ (.@ t (.transpose (first weights)))
        (second weights))
    ))

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

(define-syntax grid-search
  (syntax-rules ()
    ((grid-search done? ((param val ...) ...) . body)
     (gridsearch done? 0 () ((param val ...) ...) . body))))

(define (next-params ls params)
  (let lp ((ls (reverse ls)) (params (reverse params)) (res '()))
    (cond
     ((null? ls) (error "out of params"))
     ((pair? (cdar ls)) (append (reverse (cdr ls)) (cons (cdar ls) res)))
     (else (lp (cdr ls) (cdr params) (cons (car params) res))))))

(define-syntax gridsearch
  (syntax-rules ()
    ((gridsearch done? i (params ...) ((param val ...) . rest) . body)
     (gridsearch done? (+ i 1) (params ... (param tmp i val ...)) rest . body))
    ((gridsearch done? next-i ((param tmp i val ...) ...) () . body)
     (let ((params (list (list val ...) ...)))
       (let lp ((ls params))
         (let ((tmp (car (list-ref params i))) ...)
           (parameterize ((param tmp) ...)
             (let ((res (begin . body)))
               (if (done? res)
                   res
                   (lp (next-params ls params)))))))))))

(define (array-index-of-max a)
  (let ((getter (array-getter a))
        (start (interval-lower-bound (array-domain a) 0))
        (end (interval-upper-bound (array-domain a) 0)))
    (let lp ((i (+ start 1))
             (max-i start)
             (max-elt (getter start)))
      (cond
       ((>= i end)
        max-i)
       ((> (getter i) max-elt)
        (lp (+ i 1) i (getter i)))
       (else
        (lp (+ i 1) max-i max-elt))))))

(define (class= a1 a2)
  (let ((dim (array-dimension a1)))
    (if (not (= dim (array-dimension a2)))
        (error "class= dimensions don't match" dim (array-dimension a2)
               'test: (interval-widths (array-domain a1))
               'pred: (interval-widths (array-domain a2)))
        (case dim
          ((0) (error "not a one-hot-like array" a1))
          ((1) (if (= (array-index-of-max a1) (array-index-of-max a2)) 1. 0.))
          (else
           ;;(write `(class= ,(array->list* (array-curry a1 (- dim 1))) (array->list* (array-curry a2 (- dim 1))))) (newline)
           (array-copy
            (array-map class=
                       (array-curry a1 (- dim 1))
                       (array-curry a2 (- dim 1)))))))))

(define (accuracy model xs ys)
  (let ((pred-ys (dual-value (model xs))))
    (dbg "pred-ys: " pred-ys)
    (/ (array-sum (class= ys pred-ys))
       (interval-width (array-domain ys) 0))))
