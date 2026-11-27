
(define titanic-data
  (table-column-map-to
   (table-column-map-to
    (table-load-csv (find-module-file "chibi/ai/examples/titanic-train.csv")
                    'header-from-first-line?: #t)
    'Age
    (lambda (a) (if (equal? a "") +nan.0 (string->number a)))
    'AgeInt)
   'Sex
   (lambda (s) (if (eq? s 'male) 1 0))
   'SexInt))

(define-values (titanic-train titanic-test)
  (table-split titanic-data 0.8))

(define titanic-train-xs
  (table->array titanic-train '#(SexInt AgeInt SibSp Fare)))
(define titanic-train-ys
  (table->array titanic-train '#(Survived)))

(define titanic-test-xs
  (table->array titanic-test '#(SexInt AgeInt SibSp Fare)))
(define titanic-test-ys
  (table->array titanic-test '#(Survived)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define titanic-network
  (stack-blocks
   (list
    ;; sex,age,sibsp,fare
    (dense-block 4 6)
    (dense-block 6 1))))

(define titanic-classifier (block-fn titanic-network))

(define titanic-weight-shapes (block-ls titanic-network))

(define (accurate-enough-titanic-weights? weights)
  (>= (accuracy (model titanic-classifier weights)
                titanic-test-xs titanic-test-ys)
      .9))

(define (main args)
  (let* ((titanic-weights
          (parameterize ((max-learning-iterations 20) ;; 2000
                         (learning-rate .002)
                         (learning-batch-size 8))
            (naked-gradient-descent
             (sampling-loss (l2-loss titanic-classifier)
                            titanic-train-xs
                            titanic-train-ys)
             (init-weights titanic-weight-shapes))))
         (titanic-model (model titanic-classifier titanic-weights))
         (titanic-acc (accuracy titanic-model titanic-test-xs titanic-test-ys))
         )
    (write `(weights: ,@(map array->list* (map dual-value titanic-weights))))
    (newline)
    (write `(accuracy: ,titanic-acc))
    (newline)))
