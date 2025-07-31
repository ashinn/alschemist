
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Array Utilities

(define (array-of-dual? a)
  (and (array? a)
       (positive? (interval-volume (array-domain a)))
       (let ((elt (array-first a)))
         (or (dual? elt)
             (array-of-dual? elt)))))

(define (axes-to-unbroadcast dest from)
  (assert (<= (interval-dimension dest) (interval-dimension from)))
  (let* ((dim (interval-dimension from))
         (limit (- dim (interval-dimension dest))))
    (let lp ((i (- dim 1)) (res '()))
      (cond
       ((negative? i)
        res)
       ((< i limit)
        (lp (- i 1) (cons i res)))
       ((= (interval-width dest (- i limit)) (interval-width from i))
        (lp (- i 1) res))
       ((= (interval-width dest (- i limit)) 1)
        (lp (- i 1) (cons i res)))
       ((= (interval-width from i) 1)
        (lp (- i 1) res))
       (else
        (error "can't (un)broadcast"
               (interval-widths dest)
               (interval-widths from)))))))

;; If the array is the result of broadcasting from the domain, narrow
;; it back down by summing the broadcast axes, otherwise broadcast it
;; into the new domain.
(define (array-un/broadcast v domain)
  (cond
   ((not (array? v))
    (make-specialized-array domain generic-storage-class v))
   ((equal? (array-domain v) domain)
    v)
   (else
    (let ((v (if (specialized-array? v) v (array-copy v))))
      (dbg "unbroadcast: " v " => " domain)
      (cond
       ((and (= 1 (interval-dimension domain))
             (= 2 (interval-dimension (array-domain v)))
             (= (interval-width domain 0)
                (interval-width (array-domain v) 1)))
        (let ((res (array-sum-axis/squeeze v 0)))
          (dbg "unbroadcast result: " res)
          res))
       ((<= (interval-volume domain)
            (interval-volume (array-domain v)))
        (let lp ((ls (axes-to-unbroadcast domain (array-domain v)))
                 (v v))
          (cond
           ((null? ls)
            (dbg "unbroadcast result: " v)
            v)
           (else
            (lp (cdr ls)
                (array-sum-axis v (car ls)))))))
       (else
        (array+ (zeros domain (array-storage-class v))
                v)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Gradients}

(define (make-gradients)
  (make-hash-table eq?))
(define (gradients-done? grads d)
  (assert (dual? d))
  (hash-table-ref/default grads d #f))
(define (gradients-ref grads d)
  (assert (dual? d))
  (hash-table-ref/default grads d 0.0))
(define (gradients-set! grads d v)
  (hash-table-set! grads d v))

(define (shape-of x)
  (cond ((dual? x) (shape-of (dual-value x)))
        ((array? x) (interval-widths (array-domain x)))
        (else '())))

(define (gradients-inc! grads d v)
  (assert (dual? d))
  (unless (dual-const? d)
    (let* ((current-grad (hash-table-ref/default grads d #f))
           (v (if (promise? v) (force v) v))
           (v (if (array? (dual-value d))
                  (array-un/broadcast v (array-domain (dual-value d)))
                  v)))
      (dbg
       "inc! " (dual-format-label d #t) " " (shape-of d) " + " (shape-of v)
       " avg: " (dual-value (if (array? (dual-value v)) (.mean v) v)))
      (cond
       (current-grad
        (gradients-set! grads d (array+ current-grad v)))
       (else
        (gradients-set! grads d v)
        ((dual-link d) d grads))))))

(define (gradient loss wrt)
  (let ((grads (make-gradients)))
    (gradients-set! grads loss 1.0)
    ((dual-link loss) loss grads)
    (if (dual? wrt)
        (gradients-ref grads wrt)
        (map (lambda (d) (gradients-ref grads d)) wrt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Operators}

(define-syntax define-dual-op
  (syntax-rules ()
    ((define-dual-op (name arg ...) array-op op-name link)
     (define name
       (let ((label (symbol->string 'op-name)))
         (lambda (arg ...)
           (let ((arg (as-dual arg)) ...
                 (op-name (if (or (array-of-dual? (dual-value arg)) ...)
                              (lambda (arg ...) (array-map name arg ...))
                              array-op)))
             (dual
              (op-name (dual-value arg) ...)
              link
              (dual-op-label label arg ...)))))))))

(define-dual-op (dual+ a b) array+ +
  (lambda (self grads)
    (let ((grad (gradients-ref grads self)))
      (gradients-inc! grads a grad)
      (gradients-inc! grads b grad))))

(define-dual-op (dual* a b) array* *
  (lambda (self grads)
    (let ((grad (gradients-ref grads self)))
      (gradients-inc! grads a (* (dual-value b) grad))
      (gradients-inc! grads b (* (dual-value a) grad)))))

(define (dual- a b)
  (dual+ a (dual* -1 b)))

(define-dual-op (dual-expt a b) array-expt **
  (let ((* (if (array-of-dual? (dual-value a)) dual* array*)))
    ;; TODO: non-constant exponent
    (lambda (self grads)
      (let* ((grad (gradients-ref grads self))
             (delta (* (** (dual-value a) (- (dual-value b) 1))
                       (dual-value b)
                       grad)))
        (gradients-inc! grads a delta)))))

(define (dual/ a b)
  (dual* a (dual-expt b -1)))

(define-dual-op (dual-exp a) array-exp e^
  (lambda (self grads)
    (let* ((grad (gradients-ref grads self))
           (delta (array* (e^ (dual-value a)) grad)))
      (gradients-inc! grads a (exp grad)))))

(define-dual-op (dual-log a) array-log ln
  (lambda (self grads)
    (let* ((grad (gradients-ref grads self))
           (delta (array* (array/ (dual-value a)) grad)))
      (gradients-inc! grads a delta))))

(define (array-tanh a)
  (let ((e^2a (array-exp (array* a 2.0))))
    (array/ (array- e^2a 1.0) (array+ e^2a 1.0))))

(define (dual-tanh a)
  (let* ((a (as-dual a))
         (value (array-tanh (dual-value a))))
    (dual
     value
     (lambda (self grads)
       (let* ((grad (gradients-ref grads self))
              (delta (if (array? value)
                         (array* (array- 1.0 (array-square value)) grad)
                         (array* (- 1.0 (square value)) grad))))
         (gradients-inc! grads a delta)))
     (dual-op-label "tanh" a))))

(define (dual-rectify a)
  (let ((res (array-rectify (dual-value a)))
        (non-zeros (array-map (lambda (x) (if (zero? x) 0 1)) (dual-value a))))
    (dual
     res
     (lambda (self grads)
       (gradients-inc! grads a (array-mul-elements non-zeros (gradients-ref grads self))))
     (dual-op-label "rectify" a))))

(define (dual-sum a)
  (let ((a (as-dual a)))
    (dual
     (if (array-of-dual? (dual-value a))
         (array-fold-left
          (lambda (acc x)
            (.+ (if (array? (dual-value x)) (dual-sum x) x) acc))
          0
          (dual-value a))
         (array-sum (dual-value a)))
     (lambda (self grads)
       (let ((grad (gradients-ref grads self)))
         (gradients-inc! grads a grad)))
     (dual-op-label "sum" a))))

(define (dual-sum-axis a . o)
  (let* ((a (as-dual a))
         (a-val (dual-value a))
         (axis (if (pair? o) (car o) (- (array-dimension a-val) 1)))
         (width (interval-width (array-domain a-val) axis))
         (storage (if (specialized-array? a-val)
                      (array-storage-class a-val)
                      generic-storage-class)))
    (cond
     ((< (array-dimension a-val) 2)
      (array-sum a))
     ((> (array-dimension a-val) 2)
      (error "dual-sum-axis not yet supported on higher dimensions" a))
     ((= axis 1)
      ;; (m, n) x (n, 1) => (m, 1)
      (dual-matmul a (const (ones (make-interval (vector width 1)) storage))))
     (else
      ;; (1, m) x (m, n) => (1, n)
      (dual-matmul (const (ones (make-interval (vector 1 width)) storage)) a)))))

(define (dual-sum-axis/squeeze a . o)
  (let* ((axis (if (pair? o) (car o) (- (array-dimension (dual-value a)) 1)))
         (res (dual-sum-axis a axis)))
    (dual
     (array-squeeze (dual-value res) axis)
     (lambda (self grads)
       (let ((grad (gradients-ref grads self)))
         ;; The squeezed gradient should broadcast correctly.
         (gradients-inc! grads res grad)
         (gradients-inc! grads a grad)
         ))
     (dual-op-label "sum-axis" a))))

(define (dual-mean a)
  (dual/ (dual-sum a)
         (interval-volume (array-domain (dual-value a)))))

(define (dual-matmul a b)
  (let ((a (as-dual a)) (b (as-dual b)))
    ;; M x K @ K x N = M x N
    ;; (write `(.@ ,(pp a) ,(pp b))) (newline)
    (dbg `(@ ,a ,b))
    (let ((res (array-mul (dual-value a) (dual-value b))))
      (dual
       res
       (lambda (self grads)
         ;; (write `(.@ grad)) (newline)
         ;; The incoming gradient can un/broadcast to M x N.
         (let ((grad (array-un/broadcast
                      (dual-value (gradients-ref grads self))
                      (array-domain res))))
           ;; (write `(.@ a += ,(pp grad) ,(pp (array-transpose (dual-value b))))) (newline)
           ;; B' = N x K
           ;; M x N @ N x K => M x K, the A gradient
           (gradients-inc! grads a (delay (array-mul
                                           grad
                                           (array-transpose (dual-value b)))))
           ;; (write `(.@ b += ,(pp (array-transpose (dual-value a))) ,(pp grad))) (newline)
           ;; A' = K x M
           ;; K x M @ M x N => K x N, the B gradient
           (gradients-inc! grads b (delay (array-mul
                                           (array-transpose (dual-value a))
                                           grad)))
           ;; (write `(.@ done)) (newline)
           ))
       (dual-op-label "@" a b)))))

(define (dual-reshape a domain)
  (let ((storage (if (specialized-array? a)
                     (array-storage-class a)
                     generic-storage-class)))
    (dual (specialized-array-reshape (dual-value a) domain)
          (lambda (self grads)
            (let* ((reshaped-grad (array+ (zeros domain storage)
                                          (gradients-ref grads self)))
                   (grad (specialized-array-reshape reshaped-grad
                                                    (array-domain (dual-value a))
                                                    #t)))
              (gradients-inc! grads a grad)))
          (dual-op-label "reshape" a))))

(define (dual-transpose a)
  (case (array-dimension (dual-value a))
    ((1)
     (dual-reshape
      a
      (make-interval (vector (interval-width (array-domain (dual-value a)) 0) 1))))
    ((2)
     (dual (array-transpose (dual-value a))
           (lambda (self grads)
             (let ((grad (array-transpose (gradients-ref grads self))))
               (gradients-inc! grads a grad)))
           (dual-op-label "transpose" a)))
    (else
     (error "can only transpose arrays of dimensions 1 or 2 but got"
            (array-dimension (dual-value a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define zero (const 0.0))
(define one (const 1.0))

(define (.+ . args)
  (reduce dual+ zero args))
(define (.* . args)
  (reduce dual* one args))
(define (.- . args)
  (reduce (lambda (a b) (dual- b a)) zero args))
(define (./ . args)
  (reduce (lambda (a b) (dual/ b a)) one args))
(define .@ dual-matmul)  ;; TODO: optimized n-ary
(define .expt dual-expt)
(define .exp dual-exp)
(define .log dual-log)
(define .rectify dual-rectify)
(define .tanh dual-tanh)
(define .sum dual-sum)
(define .mean dual-mean)
(define .sum-axis dual-sum-axis)
(define .sum-axis/squeeze dual-sum-axis/squeeze)
(define .reshape dual-reshape)
(define .transpose dual-transpose)
(define (.square x) (.expt x 2))
(define (.dot x y) (.sum (.* x y)))
