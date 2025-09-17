
(define iris-network
  (stack-blocks
   (list
    (dense-block 4 8)
    (dense-block 8 3))))

(define iris-classifier (block-fn iris-network))

(define iris-weight-shapes (block-ls iris-network))

(define (accurate-enough-iris-weights? weights)
  (>= (accuracy (model iris-classifier weights)
                iris-test-xs iris-test-ys)
      .9))

(define iris-init-weights
  (init-weights iris-weight-shapes generic-storage-class iris-random-source))

(define (iris-debug xs ys pred-ys)
  (array-for-each
   (lambda (x y pred-y)
     (dbg
      (with ((precision 2))
        (each "sepal: " (array-ref x 0) " x " (array-ref x 1)
              ", petal: " (array-ref x 2) " x " (array-ref x 3)
              ", expect: " (iris-class y) ", got: " (iris-class pred-y)
              " " (array->list* pred-y)))))
   (array-rows (dual-value xs))
   (array-rows (dual-value ys))
   (array-rows (dual-value pred-ys))))

(define (main args)
  (let ((iris-train-xs (array-normalize-columns iris-train-xs (iota 4)))
        (iris-test-xs (array-normalize-columns iris-test-xs (iota 4))))
    (dbg "train-xs: " iris-train-xs)
    (dbg "train-ys: " iris-train-ys)
    (dbg "weights: " iris-init-weights)
    (parameterize ((max-learning-iterations 2000)
                   (learning-rate .01)
                   (learning-batch-size 8))
      ;; (write-string (network->torch iris-network
      ;;                               'xs: iris-train-xs
      ;;                               'ys: iris-train-ys
      ;;                               'ws: iris-init-weights))
      (let* (;; (iris-torch-output
             ;;  (network->torch-output iris-network
             ;;                         'xs: iris-train-xs
             ;;                         'ys: iris-train-ys
             ;;                         'ws: iris-init-weights))
             ;; (iris-torch-weights
             ;;  (map tensor (cdr (assq 'weights iris-torch-output))))
             (iris-weights
              (naked-gradient-descent ;;velocity-gradient-descent ;;
               (sampling-loss (l2-loss iris-classifier iris-debug)
                              iris-train-xs
                              iris-train-ys
                              iris-random-source)
               ;; ((l2-loss iris-classifier iris-debug)
               ;;  iris-train-xs
               ;;  iris-train-ys)
               iris-init-weights))
             (iris-model (model iris-classifier iris-weights))
             (iris-acc (accuracy iris-model iris-test-xs iris-test-ys
                                 iris-debug)))
        (dbg "final weights: " iris-weights)
        (info "accuracy: " iris-acc)))))
