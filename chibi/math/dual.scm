    
;;> \section{Duals}

(define-record-type Dual
  (make-dual value link label const?)
  dual?
  (value %dual-value)
  (link dual-link)
  (label %dual-label)
  (const? dual-const?))

(define (dual value . o)
  (let ((link (if (pair? o) (car o) values))
        (label (and (pair? o) (pair? (cdr o)) (cadr o))))
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

(define (dual-label x) (and (dual? x) (%dual-label x)))

(define (dual-with-label value label)
  (if (dual? value)
      (make-dual (dual-value value) (dual-link value) label (dual-const? value))
      (make-dual value values label #f)))

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

(define (dual-format-label x . o)
  (define (->string x)
    (cond ((string? x) x)
          ((symbol? x) (symbol->string x))
          ((number? x) (number->string x))
          (else "_")))
  (let* ((label (if (dual? x) (dual-label x) x))
         (recurse? (and (pair? o) (car o)))
         (format-arg (if recurse?
                         (lambda (x) (dual-format-label x #t))
                         ->string)))
    (cond
     ((pair? label)
      (if (= 3 (length label))
          ;; most ops are binary so we format them as infix
          (string-append
           (format-arg (cadr label))
           (->string (car label))
           (format-arg (car (cddr label))))
          ;; general ops are formatted as function calls
          (let ((out (open-output-string)))
            (write-string (->string (car label)) out)
            (write-string "(" out)
            (pair-for-each
             (lambda (x)
               (if (not (eq? x (cdr label)))
                   (write-string "," out))
               (write-string (format-arg (car x)) out))
             (cdr label))
            (write-string ")" out)
            (get-output-string out))))
     (else (->string label)))))
