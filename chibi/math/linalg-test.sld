
(define-library (chibi math linalg-test)
  (import (scheme base) (scheme list)
          (srfi 231)
          (chibi math linalg) (chibi test))
  (export run-tests)
  (begin
    (define (depth ls)
      (if (list? ls) (+ 1 (depth (car ls))) 0))
    (define (tensor nested-ls . o)
      (apply list*->array (depth nested-ls) nested-ls o))
    (define-syntax test-array
      (syntax-rules ()
        ((test-array expected expr)
         (test-equal array= expected expr))))
    (define (run-tests)
      (test-begin "linear algebra")
      (test 10
          (determinant (tensor '((4 1) (2 3)))))
      (test -6
          (determinant (tensor '((-2 3 -1) (5 -1 4) (4 -8 2)))))
      (test-array (tensor '((4 7) (2 6) (1 0) (0 1)))
        (array-append 0 (list (tensor '((4 7) (2 6))) (identity-array 2))))
      (test-array (tensor '((4 7) (2 6) (1 0) (0 1)))
        (array-append 0
                      (list (list->array (make-interval '#(2 2) '#(4 4))
                                         '(4 7 2 6))
                            (identity-array 2))))
      (test-array (tensor '((4 7 1 0) (2 6 0 1)))
        (array-append 1 (list (tensor '((4 7) (2 6))) (identity-array 2))))
      (test-array (tensor '((4 7 2 1 0) (6 3 5 0 1)))
        (array-append 1 (list (tensor '((4 7 2) (6 3 5))) (identity-array 2))))
      (test-array (tensor '((4 7 1 0 0 1 3)
                            (2 6 0 1 5 8 9)))
        (array-append
         1
         (list (list->array (make-interval '#(2 2)) '(4 7 2 6))
               (identity-array 2)
               (list->array (make-interval '#(2 3)) '(0 1 3 5 8 9)))))
      (test-array (tensor '(((4 7) (2 6))
                            ((1 0) (0 1))))
        (array-stack 0 (list (tensor '((4 7) (2 6))) (identity-array 2))))
      (test-array (tensor '(((4 7) (1 0))
                            ((2 6) (0 1))))
        (array-stack 1 (list (tensor '((4 7) (2 6))) (identity-array 2))))
      (test-array (tensor '(((4 1) (7 0))
                            ((2 0) (6 1))))
        (array-stack 2 (list (tensor '((4 7) (2 6))) (identity-array 2))))
      (test-array (tensor '((3/5 -7/10) (-1/5 2/5)))
        (array-inverse (tensor '((4 7) (2 6)))))
      (test-array (tensor '((1/5 1/5 0)
                            (-1/5 3/10 1)
                            (1/5 -3/10 0)))
        (array-inverse
         (tensor '((3 0 2)
                   (2 0 -2)
                   (0 1 1)))))
      (test-array (tensor '((1 2) (3 4)))
        (array-mul (tensor '((1 2) (3 4)))
                   (tensor '((1 0) (0 1)))))
      (test-array (tensor '((1 2) (3 4)))
        (array-mul (tensor '((1 0) (0 1)))
                   (tensor '((1 2) (3 4)))))
      (test-array (tensor '((4 4) (10 8)))
        (array-mul (tensor '((1 2) (3 4)))
                   (tensor '((2 0) (1 2)))))
      (test-array (tensor '((4 4) (10 8)) u64-storage-class)
        (array-mul (tensor '((1 2) (3 4)) u64-storage-class)
                   (tensor '((2 0) (1 2)) u64-storage-class)))
      (test-array (tensor '((4. 4.) (10. 8.)) f32-storage-class)
        (array-mul (tensor '((1. 2.) (3. 4.)) f32-storage-class)
                   (tensor '((2. 0.) (1. 2.)) f32-storage-class)))
      (test-array (tensor '((4-2i 4.) (10. 8.)) c64-storage-class)
        (array-mul (tensor '((1-i 2.) (3. 4.)) c64-storage-class)
                   (tensor '((2. 0.) (1. 2.)) c64-storage-class)))
      (test-array (tensor '((58 64) (139 154)))
        (array-mul (tensor '((1 2 3) (4 5 6)))
                   (tensor '((7 8) (9 10) (11 12)))))
      (test-array (array-mul (tensor '((1 2 3 4 5)) s16-storage-class)
                             (array-mul
                              (tensor '((1 2) (3 4) (5 6) (7 8) (9 0))
                                      s16-storage-class)
                              (tensor '((1 2 3 4 5) (6 7 8 9 0))
                                      s16-storage-class)))
        (array-mul (tensor '((1 2 3 4 5)))
                   (tensor '((1 2) (3 4) (5 6) (7 8) (9 0)))
                   (tensor '((1 2 3 4 5) (6 7 8 9 0)))))
      (test-array (tensor '((4 7)))
        (array-mul (tensor '((1. 2.)) f32-storage-class)
                   (tensor '((0. 1.) (2. 3.)) f32-storage-class)))
      (let ((C (tensor '((0. 0.) (0. 0.)) f64-storage-class)))
        (array-mul! C
                    (tensor '((1. 2.) (3. 4.)) f64-storage-class)
                    (tensor '((2. 0.) (1. 2.)) f64-storage-class))
        (test-array (tensor '((4. 4.) (10. 8.)) f64-storage-class)
          (values C)))
      (test-array (tensor '((1 0) (0 1)))
        (array-expt (tensor '((1 0) (0 1))) 7))
      (test-array (tensor '((37 54) (81 118)))
        (array-expt (tensor '((1 2) (3 4))) 3))
      (let ((A (tensor '((1 2) (3 4)))))
        (array-add-elements! A 1)
        (test-array (tensor '((2 3) (4 5))) (values A))
        (array-sub-elements! A 2)
        (test-array (tensor '((0 1) (2 3))) (values A))
        (array-mul-elements! A 3)
        (test-array (tensor '((0 3) (6 9))) (values A))
        (array-div-elements! A 2)
        (test-array (tensor '((0 3/2) (3 9/2))) (values A)))
      (let ((A (tensor '((1. 2.) (3. 4.)) f32-storage-class)))
        (array-add-elements! A 1.)
        (test-array (tensor '((2. 3.) (4. 5.))) (values A))
        (array-sub-elements! A 2.)
        (test-array (tensor '((0. 1.) (2. 3.))) (values A))
        (array-mul-elements! A 3.)
        (test-array (tensor '((0. 3.) (6. 9.))) (values A))
        (array-div-elements! A 2.)
        (test-array (tensor '((0. 1.5) (3. 4.5))) (values A)))
      (let ((A (tensor '((0. 2.) (3. 4.)) f32-storage-class)))
        (array-exp-elements! A)
        (test '((1.0 7.389056205749512)
                (20.08553695678711 54.598148345947266))
            (array->list* A))
        (array-log-elements! A)
        (test '((0. 2.) (3. 4.)) (array->list* A)))
      (test 30.
          (array-dot (tensor '(1. 2. 3. 4.))
                     (tensor '(1. 2. 3. 4.))))
      (test 30.
          (array-dot (tensor '(1. 2. 3. 4.) f32-storage-class)
                     (tensor '(1. 2. 3. 4.) f32-storage-class)))
      (test-array (tensor '(2.5))
        (array-convolve (tensor '(1. 2. 3.))
                        (tensor '(0. 1. 0.5))))
      (test-array (tensor '((145. 108.) (108. 121.)))
        (array-convolve (tensor '((3. 9. 0.) (2. 8. 1.) (1. 4. 8.)))
                        (tensor '((8. 9.) (4. 4.)))))
      (test 3 (array-sum (tensor '(1 2))))
      (test -1 (array-sum (tensor '(1 -2))))
      (test 3 (array-1norm (tensor '(1 2))))
      (test 3 (array-1norm (tensor '(1 -2))))
      (test 2.236068 (array-2norm (tensor '(1 2))))
      (test 2 (array-inf-norm (tensor '(1 2))))
      (test (expt 9. 1/3) (array-norm (tensor '(1 2)) 3))
      (test "  0 10\n200  3\n"
          (pretty-print-array (tensor '((0 10) (200 3))) #f))
      (test "[0 1]\n[2 3]\n"
          (pretty-print-array (tensor '((0 1) (2 3))) #f
                              'left: "[" 'right: "]"))
      (test "0 1\n2 3\n\n4 5\n6 7\n\n"
          (pretty-print-array (tensor '(((0 1) (2 3))
                                        ((4 5) (6 7))))
                              #f))
      (test-end))))

;; Local Variables:
;; eval: (put 'test-array 'scheme-indent-function 1)
;; End:
