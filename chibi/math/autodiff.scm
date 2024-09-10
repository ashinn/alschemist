
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> \section{Duals}

(define-record-type Dual
  (make-dual value link label const?)
  dual?
  (value %dual-value)
  (link dual-link)
  (label dual-label)
  (const? dual-const?))

(define (dual value . o)
  (let ((link (if (pair? o) (car o) values))
        (label (and (pair? o) (pair? (car o)) (cadr o))))
    (if (dual? value)
        (make-dual (dual-value value)
                   (lambda (self grads)
                     ((dual-link value) self grads)
                     (link self grads))
                   label
                   #f)
        (make-dual value link label #f))))

(define (as-dual value)
  (if (dual? value) value (dual value)))

(define (const value . o)
  (make-dual value values (and (pair? o) (car o)) #t))

(define (dual-value x) (if (dual? x) (%dual-value x) x))

(define (array-of-dual? a)
  (and (array? a)
       (positive? (interval-volume (array-domain a)))
       (let ((elt (array-first a)))
         (or (dual? elt)
             (array-of-dual? elt)))))

(define-syntax let-duals
  (syntax-rules ()
    ((let-duals ((name expr) ...) . body)
     (let ((name (let ((tmp expr))
                   (if (or (number? tmp) (array? tmp))
                       (dual tmp values 'name)
                       tmp)))
           ...)
       . body))))

(define-syntax let*-duals
  (syntax-rules ()
    ((let*-duals ((name expr) ...) . body)
     (let* ((name (let ((tmp expr))
                    (if (or (number? tmp) (array? tmp))
                        (dual tmp values 'name)
                        tmp)))
            ...)
       . body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Array Utilities

(define (axes-to-unbroadcast dest from)
  (assert (= (interval-dimension dest) (interval-dimension from)))
  (let ((dim (interval-dimension dest)))
    (let lp ((i 0) (res '()))
      (cond
       ((= i dim)
        (reverse res))
       ((= (interval-width dest i) (interval-width from i))
        (lp (+ i 1) res))
       ((= (interval-width dest i) 1)
        (lp (+ i 1) (cons i res)))
       ((= (interval-width from i) 1)
        (lp (+ i 1) res))
       (else
        (error "can't (un)broadcast" dest from))))))

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
      (cond
       ((and (< (interval-volume domain)
                (interval-volume (array-domain v))))
        (let lp ((ls (axes-to-unbroadcast domain (array-domain v)))
                 (v v))
          (cond
           ((null? ls)
            v)
           (else
            (lp (cdr ls) (array-sum-axis v (car ls)))))))
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

(define (gradients-inc! grads d v)
  (assert (dual? d))
  (unless (dual-const? d)
    (let ((current-grad (hash-table-ref/default grads d #f))
          (v (if (array? (dual-value d))
                 (array-un/broadcast v (array-domain (dual-value d)))
                 v)))
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
     (define (name arg ...)
       (let ((arg (as-dual arg)) ...
             (op-name (if (or (array-of-dual? (dual-value arg)) ...)
                          (lambda (arg ...) (array-map name arg ...))
                          array-op)))
         (dual
          (op-name (dual-value arg) ...)
          link))))))

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
         (gradients-inc! grads a delta))))))

(define (dual-relu a)
  (let* ((value (array-relu (dual-value a)))
         (ones (if (array? (dual-value a))
                   (array-copy (array-map (lambda (x) (if (zero? x) x 1))
                                          (dual-value a))
                               (if (specialized-array? (dual-value a))
                                   (array-storage-class (dual-value a))
                                   generic-storage-class))
                   1)))
    (dual
     value
     (lambda (self grads)
       (gradients-inc! grads a (array* ones (gradients-ref grads self)))))))

(define (dual-sum a)
  (let ((a (as-dual a)))
    (dual
     (if (array-of-dual? (dual-value a))
         (array-fold-left .+ 0 (dual-value a))
         (array-sum (dual-value a)))
     (lambda (self grads)
       (let ((grad (gradients-ref grads self)))
         (gradients-inc! grads a grad))))))

(define (dual-sum-axis a . o)
  (let* ((a (as-dual a))
         (a-val (dual-value a))
         (axis (if (pair? o) (car o) (- (array-dimension a-val) 1)))
         (widths (let ((w (interval-widths (array-domain a-val))))
                   (vector-set! w axis 1)
                   w))
         (storage (if (specialized-array? a-val)
                      (array-storage-class a-val)
                      generic-storage-class)))
    (dual-matmul a (ones (make-interval widths) storage one))))

(define (dual-mean a)
  (dual/ (dual-sum a)
         (interval-volume (array-domain (dual-value a)))))

(define (dual-matmul a b)
  (let ((a (as-dual a)) (b (as-dual b)))
    (dual
     (array-mul (dual-value a) (dual-value b))
     (lambda (self grads)
       (gradients-inc! grads a (array-mul (gradients-ref grads self)
                                          (array-transpose (dual-value b))))
       (gradients-inc! grads b (array-mul (array-transpose (dual-value a))
                                          (gradients-ref grads self)))))))

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
(define .relu dual-relu)
(define .tanh dual-tanh)
(define .sum dual-sum)
(define .mean dual-mean)
(define .sum-axis dual-sum-axis)
(define (.square x) (.expt x 2))
(define (.dot x y) (.sum (.* x y)))
