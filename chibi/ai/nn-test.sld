
(define-library (chibi ai nn-test)
  (import (scheme base) (scheme list) (scheme write)
          (srfi 27) (srfi 231)
          (chibi math autodiff) (chibi math linalg)
          (chibi math array-test-utils)
          (chibi ai nn) (chibi test))
  (export run-tests)
  (begin
    (define test-random-source
      (let ((rs (make-random-source)))
        (random-source-pseudo-randomize! rs 23 42)
        rs))
    (define (line x)
      (lambda (weights)
        (.+ (.* (list-ref weights 0) x)
            (list-ref weights 1))))
    (define (plane x)
      (lambda (weights)
        (.+ (.dot (list-ref weights 0) x)
            (list-ref weights 1))))
    (define (run-tests)
      (test-begin "(chibi ai nn)")
      (let ((line-xs (tensor '((2.) (1.) (4.) (3.))))
            (line-ys (tensor '(1.8 1.2 4.2 3.3))))
        (test 8.3025 ;; 33.21
            (dual-value
             (((l2-loss line) line-xs line-ys)
              (list 0. 0.))))
        (test 1.3801626 ;; 5.52
            (dual-value
             (((l2-loss line) line-xs line-ys)
              (list 0.6263 0.))))
        (parameterize ((max-learning-iterations 25)
                       (current-test-epsilon 1.))
          (let ((weights (naked-gradient-descent
                          ((l2-loss plane) line-xs line-ys)
                          (list (tensor '(0.)) 0.))))
            (test-array (tensor '(1.05)) (dual-value (first weights)))
            (test 1.87e-6 (dual-value (second weights))))))
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
                (dual-value (second weights))))))
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
      (let ((w (tensor '((2. 1. 3.1) (3.7 4. 6.1))))
            (t (tensor '(1.3 .4 3.3))))
        (test-array (tensor '((1.3) (.4) (3.3)))
          (dual-value (.transpose t)))
        (test-array (tensor '(13.23 26.54))
          (dual-value ((linear t) (list w 0.)))))
      (let ((layer1 (dense-block 32 64))
            (layer2 (dense-block 64 45))
            (layer3 (dense-block 45 26)))
        (let ((block (stack-blocks (list layer1 layer2 layer3))))
          (test '((64 32)
                  (64)  ; 1
                  (45 64)
                  (45)  ; 1
                  (26 45)
                  (26)) ; 1
              (block-ls block))))
      (test 1. (class= (tensor '(0. 1. 0.)) (tensor '(0.1 0.8 0.1))))
      (test 0. (class= (tensor '(0. 0. 1.)) (tensor '(0.1 0.8 0.1))))
      (test-array (tensor '(0. 1. 1.))
        (class= (tensor '((0. 0. 1.) (0. 1. 0.) (1. 0. 0.)))
                (tensor '((4. 3. 3.) (0.1 0.8 0.1) (4. 3. 3.)))))
      (test-end))))
