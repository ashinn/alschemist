
;; returns a list of two primes summing to n,
;; or #f if no such pair exists
(define (as-sum-of-2-primes n)
  (assert (even? n))
  (let ((limit (quotient n 2)))
    (let lp ((i 2))
      (cond
       ((> i limit) #f)
       ((prime? (- n i)) (list i (- n i)))
       (else (lp (prime-above i)))))))

;; raises an error if we find an integer n, lo <= n < hi,
;; such that n cannot be expressed as the sum of two primes
(define (check-sum-of-2-primes lo hi)
  ;; naive loop
  (assert (even? lo))
  (do ((n lo (+ n 2)))
      ((> n hi))
    (unless (as-sum-of-2-primes n)
      (display "not a sum of 2 primes: ")
      (write n)
      (newline)
      (error "not a sum of 2 primes" n))))

;; table - much slower
'(define (check-sum-of-2-primes lo hi)
  (assert (even? lo))
  (let ((bits (make-u1vector (quotient (- hi lo -1) 2)))
        (limit (quotient hi 2)))
    (do ((i 3 (prime-above i)))
        ((> i limit))
      (do ((j i (prime-above j)))
          ((>= j (- hi i)))
        (u1vector-set! bits (quotient (- (+ i j) lo) 2) 1)))
    (do ((i 0 (+ i 1)))
        ((>= i (u1vector-length bits)))
      (when (zero? (u1vector-ref bits i))
        (display "not a sum of 2 primes: ")
        (write (* i 2))
        (newline)
        (error "not a sum of 2 primes" (* i 2))))))

(define (main args)
  (let ((limit (or (and (pair? (cdr args)) (number->string (cadr args)))
                   10000)))
    (check-sum-of-2-primes 4 limit)
    (write-string (string-append "all integers below "
                                 (number->string limit)
                                 " are the sum of 2 primes\n"))))
