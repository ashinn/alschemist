
(define (xgb-create-booster dmat . o)
  (define (->string x)
    (cond
     ((string? x) x)
     ((symbol? x) (string-join (string-split (string-trim-right (symbol->string x) #\:) #\-) "_"))
     (else (call-with-output-string (lambda (out) (display x out))))))
  (let ((res (%xgb-booster-create dmat)))
    (let lp ((ls (if (pair? o) (car o) '())))
      (cond
       ((null? ls) res)
       ((null? (cdr ls)) (error "parameter without value" ls))
       (else
        (let ((key (->string (car ls)))
              (value (->string (cadr ls))))
          (xgb-booster-set-param! res key value)
          (lp (cddr ls))))))))
