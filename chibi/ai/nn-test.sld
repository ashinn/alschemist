
(define-library (chibi ai nn-test)
  (import (scheme base) (scheme list)
          (srfi 27) (srfi 231)
          (chibi math autodiff) (chibi math linalg)
          (chibi ai nn) (chibi test))
  (export run-tests)
  (begin
    (define test-random-source
      (let ((rs (make-random-source)))
        (random-source-pseudo-randomize! rs 23 42)
        rs))
    (define (depth ls)
      (if (list? ls) (+ 1 (depth (car ls))) 0))
    (define (tensor nested-ls . o)
      (list*->array (depth nested-ls)
                    nested-ls
                    (if (pair? o) (car o) f32-storage-class)))
    (define (array-approx= a . arrays)
      (and (array? a)
           (every array? arrays)
           (every (lambda (b) (interval= (array-domain a) (array-domain b)))
                  arrays)
           (apply array-every test-equal? a arrays)))
    (define-syntax test-array
      (syntax-rules ()
        ((test-array expected expr)
         (test-equal array-approx= expected expr))))
    (define (line x)
      (lambda (weights)
        (.+ (.* (list-ref weights 0) x)
            (list-ref weights 1))))
    (define (plane x)
      (lambda (weights)
        (.+ (.dot (list-ref weights 0) x)
            (list-ref weights 1))))
    (define (run-tests)
      (test-begin "(chibi nn)")
      (let ((plane-xs (tensor '((1. 2.05)
                                (1. 3.)
                                (2. 2.)
                                (2. 3.91)
                                (3. 6.13)
                                (4. 8.09))))
            (plane-ys (tensor '(13.99 15.99 18. 22.4 30.2 37.94))))
        (parameterize ((max-learning-iterations 25)
                       (current-test-epsilon 1.))
          (let ((weights (naked-gradient-descent
                          ((l2-loss plane) plane-xs plane-ys)
                          (list (tensor '(0. 0.)) 0.))))
            (test-array (tensor '(3.98 1.97))
              (dual-value (first weights)))
            (test 6.16
                (dual-value (second weights)))))
        ;; Half the loss in the same number of iterations.
        (parameterize ((max-learning-iterations 25)
                       (current-test-epsilon .5))
          (let ((weights (velocity-gradient-descent
                          ((l2-loss plane) plane-xs plane-ys)
                          (list (tensor '(0. 0.)) 0.))))
            (test-array (tensor '(3.98 1.97))
              (dual-value (first weights)))
            (test 6.16
                (dual-value (second weights)))))
        ;; Ideally sampling should reduce the time/memory.
        (parameterize ((max-learning-iterations 25)
                       (learning-batch-size 3)
                       (current-test-epsilon 1.))
          (let ((weights (velocity-gradient-descent
                          (sampling-loss (l2-loss plane)
                                         plane-xs
                                         plane-ys
                                         test-random-source)
                          (list (tensor '(0. 0.)) 0.))))
            (test-array (tensor '(3.98 1.97))
              (dual-value (first weights)))
            (test 6.16
                (dual-value (second weights)))))
        (parameterize ((max-learning-iterations 25)
                       (current-test-epsilon 1.))
          (let ((weights (adam-gradient-descent
                          ((l2-loss plane) plane-xs plane-ys)
                          (list (tensor '(0. 0.)) 0.))))
            (test-array (tensor '(3.98 1.97))
              (dual-value (first weights)))
            (test 6.16
                (dual-value (second weights)))))
        )
      (test-end))))
