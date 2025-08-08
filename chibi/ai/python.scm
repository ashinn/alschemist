
;; A block corresponds to a pytorch nn.Module (tf.keras.Model in
;; tensorflow).  The loss function translates directly, and the
;; optimizer is the call to gradient descent.  We can use
;; keywords/options to select among the loss functions and optimizers.

;; Python is written to output an alist of information along with the
;; final weights, which can be loaded back and evaluated from Scheme.
;; Modes of operations include training and evaluated in Scheme and/or
;; Python separately, and running and comparing fixed steps in both
;; for debugging.

(define (block-from-name name shape)
  (cond
   ((eq? 'relu name)
    (dense-block (second (first shape)) (first (first shape))))
   ((and (pair? name) (eq? 'compose (car name)))
    (let* ((len (second name))
           (a (third name))
           (b (fourth name)))
      (stack-blocks (list (block-from-name a (take shape len))
                          (block-from-name b (drop shape len))))))
   (else
    (error "can't restore block from name" name))))

(define (block-decompose block)
  (let ((name (block-name block)))
    (assert (and (pair? name) (eq? 'compose (car name))))
    (let* ((len (second name))
           (a (third name))
           (b (fourth name)))
      (values (block-from-name a (take (block-ls block) len))
              (block-from-name b (drop (block-ls block) len))))))

(define block->torch
  (opt-lambda (block weight-names)
    (let ((name (block-name block)))
      (cond
       ((eq? 'relu name)
        (py-expr
         `(nn.Sequential
           (nn.Linear ,(second (first (block-ls block)))
                      ,(first (first (block-ls block))))
           (nn.ReLU))))
       ((pair? name)
        (case (first name)
          ((compose)
           (let* ((len (second name))
                  (a-weights (and weight-names (take weight-names len)))
                  (b-weights (and weight-names (drop weight-names len))))
             (let-values (((block-a block-b)
                           (block-decompose block)))
               (py-expr
                `(nn.Sequential
                  ,(block->torch block-a a-weights)
                  ,(block->torch block-b b-weights))))))
          (else
           (error "don't know how to compile block to torch" block))))
       (else
        (error "don't know how to compile block to torch" block))))))

;;> For the given (chibi ai nn) block, returns a Python program as a
;;> string implementing the equivalvent network in pytorch.
(define (network->torch block . o)
  (let-keywords* o ((xs #f)
                    (ys #f)
                    (ws #f)
                    (loss '(nn.MSELoss (= reduction "mean")))
                    (optimizer '(torch.optim.Adam (model.parameters)
                                                  (= lr learning_rate))))
    (let* ((num-weights (length (block-ls block)))
           (weight-names
            (map (lambda (i)
                   (string->symbol (string-append "w" (number->string i))))
                 (iota num-weights))))
      (show #f
            (with ((precision 4)
                   (tensor-dialect 'torch))
              (py-expr
               `(%begin
                 ;; Imports and utility definitions.
                 (import numpy np)
                 (import torch)
                 (import torch.nn nn)
                 (import torch.nn.functional F)
                 (def (print_tensor_as_sexp t)
                      (if (not (torch.is_tensor t))
                          (print t)
                          (== 1 t.ndim)
                        (print (%f"({' '.join([str(x.item()) for x in t])})"))
                        (%begin
                         (print "(")
                         (for row t
                              (print " " (= end ""))
                              (print_tensor_as_sexp row))
                         (print ")"))))
                 (def (layer_with_weights layer weights (= bias None))
                      (layer.weight.data.copy_ weights)
                      (if bias
                          (layer.bias.data.copy_ bias))
                      (return layer))
                 (torch.manual_seed 42)
                 (torch.cuda.manual_seed 42)
                 ;; Initialize
                 (= learning_rate ,(learning-rate))
                 (= batch_size ,(learning-batch-size))
                 ,@(if xs `((= xs ,(py-array xs #t))) '())
                 ,@(if ys `((= ys ,(py-array ys #t))) '())
                 ,@(if ws
                       (map (lambda (name weight)
                              `(= ,name ,weight))
                            weight-names
                            ws)
                       '())
                 (= losses #())
                 ;; Run gradient descent
                 (try
                  (= model ,(block->torch block (and ws weight-names)))
                  (= loss_fn ,loss)
                  (= optimizer ,optimizer)
                  (for epoch (range 0 ,(max-learning-iterations))
                       (= rand_indices
                          (%slice (torch.randperm (xs.size 0))
                                  #f batch_size))
                       (= ys_pred (model (vector-ref xs rand_indices)))
                       (= loss (loss_fn ys_pred (vector-ref ys rand_indices)))
                       (losses.append loss)
                       (optimizer.zero_grad)
                       (loss.backward)
                       (optimizer.step))
                  (= (%begin ,@weight-names) (model.parameters))
                  (except (Exception exn)
                          (print (%f"torch model failed: {exn}"))))
                 ;; Output
                 (try
                  (print "((loss")
                  (for loss losses (print (%f"  {loss}")))
                  (print "  )")
                  (print " (weights")
                  (for weight #(,@weight-names)
                       (print_tensor_as_sexp weight))
                  (print "  ))")
                  (except (Exception exn)
                          (print (%f"weight printing failed: {exn}"))))))))
      )))

;;> Compiles the block to pytorch with \scheme{network->torch}, runs
;;> it, and returns the output as an alist.
(define (network->torch-output block . o)
  (let-keywords* o ((python default-python-binary)
                    rest)
    (let ((code (apply network->torch block rest)))
      (let-values (((out err) (shell->output&error ,python (<< ,code))))
        (when (and err (not (equal? err "")))
          (write-string err (current-error-port))
          (newline (current-error-port)))
        (guard (exn
                (else
                 (warn "invalid torch output, expected a sexp" out exn)
                 '()))
          (read (open-input-string out)))))))
