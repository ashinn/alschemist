
(define-library (chibi math linalg-test)
  (import (scheme base) (scheme list)
          (srfi 179)
          (chibi math linalg) (chibi test))
  (export run-tests)
  (begin
    (define (flatten ls)
      (if (pair? (car ls))
          (append-map flatten ls)
          ls))
    (define (tensor nested-ls)
      (let lp ((ls nested-ls) (lens '()))
        (cond
         ((pair? ls) (lp (car ls) (cons (length ls) lens)))
         (else
          (list->array (flatten nested-ls)
                       (make-interval (list->vector (reverse lens))))))))
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
        (array-concatenate 0 (tensor '((4 7) (2 6))) (identity-array 2)))
      (test-array (tensor '((4 7) (2 6) (1 0) (0 1)))
        (array-concatenate 0
                           (list->array '(4 7 2 6)
                                        (make-interval '#(2 2) '#(4 4)))
                           (identity-array 2)))
      (test-array (tensor '((4 7 1 0) (2 6 0 1)))
        (array-concatenate 1 (tensor '((4 7) (2 6))) (identity-array 2)))
      (test-array (tensor '((4 7 2 1 0) (6 3 5 0 1)))
        (array-concatenate 1 (tensor '((4 7 2) (6 3 5))) (identity-array 2)))
      (test-array (tensor '((4 7 1 0 0 1 3)
                            (2 6 0 1 5 8 9)))
        (array-concatenate
         1
         (list->array '(4 7 2 6) (make-interval '#(2 2)))
         (identity-array 2)
         (list->array '(0 1 3 5 8 9) (make-interval '#(2 3)))))
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
      (test-array (tensor '((58 64) (139 154)))
        (array-mul (tensor '((1 2 3) (4 5 6)))
                   (tensor '((7 8) (9 10) (11 12)))))
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
