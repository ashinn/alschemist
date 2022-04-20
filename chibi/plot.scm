
(define-record-type Plot-Element
  (make-plot-element data function attributes)
  plot-element?
  (data plot-element-data plot-element-data-set!)
  (function plot-element-function plot-element-function-set!)
  (attributes plot-element-attributes plot-element-attributes-set!))

(define-record-type Plot
  (make-plot elements attributes)
  plot?
  (elements plot-elements)
  (attributes plot-attributes))

(define (->string x)
  (if (string? x)
      x
      (let ((out (open-output-string)))
        (write x out)
        (get-output-string out))))

(define (assq-ref key ls . o)
  (cond ((assq key ls) => cdr)
        (else (and (pair? o) (car o)))))

(define (array-column array col)
  (array-ref (array-curry (array-permute array '#(1 0)) 1) col))

(define (array-with-column array col vals)
  (let* ((num-rows (- (interval-upper-bound (array-domain array) 0)
                      (interval-lower-bound (array-domain array) 0)))
         (num-cols (- (interval-upper-bound (array-domain array) 1)
                      (interval-lower-bound (array-domain array) 1)))
         (vals (specialized-array-reshape
                vals
                (make-interval (vector num-rows 1)))))
    (cond
     ((zero? col)
      (array-append
       1
       ;; new col
       vals
       ;; right of col
       (array-extract array
                      (make-interval (vector 0 (+ col 1))
                                     (vector num-rows num-cols)))))
     ((= (+ col 1) num-cols)
      (array-append
       1
       ;; left of col
       (array-extract array (make-interval (vector num-rows col)))
       ;; new col
       vals))
     (else
      (array-append
       1
       ;; left of col
       (array-extract array (make-interval (vector num-rows col)))
       ;; new col
       vals
       ;; right of col
       (array-extract array
                      (make-interval (vector 0 (+ col 1))
                                     (vector num-rows num-cols))))))))

(define (plot-element-values pe)
  (let ((data (plot-element-data pe))
        (attributes (plot-element-attributes pe)))
    (if (and (array? data) (> (array-dimension data) 1))
        (let ((y-col (or (assq-ref 'y-col: attributes)
                         (let ((x-col (assq-ref 'x-col: attributes))
                               (label-col (assq-ref 'label-col: attributes)))
                           (cond
                            ((or x-col label-col)
                             (modulo (+ (max (or x-col 0) (or label-col 0))
                                        1)
                                     (array-dimension data)))
                            (else
                             0))))))
          (array-column data y-col))
        data)))

(define (plot-element-with-values pe vals)
  (let ((data (plot-element-data pe))
        (attributes (plot-element-attributes pe)))
    (make-plot-element
     (if (and (array? data) (> (array-dimension data) 1))
         (let ((y-col (or (assq-ref 'y-col: attributes)
                          (let ((x-col (assq-ref 'x-col: attributes))
                                (label-col (assq-ref 'label-col: attributes)))
                            (cond
                             ((or x-col label-col)
                              (modulo (+ (max (or x-col 0) (or label-col 0))
                                         1)
                                      (array-dimension data)))
                             (else
                              0))))))
           (array-with-column data y-col vals))
         vals)
     #f
     (plot-element-attributes pe))))

(define (plot-element-interval pe)
  (cond
   ((and (plot-element? pe) (plot-element-function pe))
    '(-1 . 1))  ;; TODO: base on the function
   ((array? (plot-element-data pe))
    (let ((values (plot-element-values pe)))
      (cons (array-fold min +inf.0 values)
            (array-fold max -inf.0 values))))
   (else
    (cons (minimum (if (plot-element? pe) (plot-element-data pe) pe))
          (maximum (if (plot-element? pe) (plot-element-data pe) pe))))))

(define (interval-boundary intervals . o)
  (let ((include-origin? (and (pair? o) (car o))))
    (let lp ((ls intervals) (lo +inf.0) (hi -inf.0))
      (if (null? ls)
          (let* ((lo2 (if include-origin? (min 0 lo) lo))
                 (hi2 (if include-origin? (max 0 hi) hi))
                 (height (- hi2 lo2))
                 (lo3 (if (finite? lo) (- lo (* 0.2 height)) lo))
                 (hi3 (if (finite? hi) (+ hi (* 0.2 height)) hi)))
            (cons (min lo2 lo3) (max hi2 hi3)))
          (lp (cdr ls) (min (caar ls) lo) (max (cdar ls) hi))))))

(define (plist->alist ls)
  (let lp ((ls ls) (res '()))
    (cond
     ((null? ls) (reverse res))
     ((null? (cdr ls)) (error "missing value for attribute" ls))
     (else (lp (cddr ls) (cons (cons (car ls) (cadr ls)) res))))))

(define (plot/function f . attrs)
  (make-plot-element #f f (plist->alist attrs)))

(define (plot/data data . attrs)
  (make-plot-element data #f (plist->alist attrs)))

(define (plot/lines data . attrs)
  (make-plot-element data #f (cons '(style: . lines) (plist->alist attrs))))

(define (plot/histogram data . attrs)
  (make-plot-element data #f (cons '(style: . histogram) (plist->alist attrs))))

(define (plot-function->data pe x-range width)
  (let* ((x-range (or x-range '(-1 . 1)))
         (incr (/ (- (cdr x-range) (car x-range)) width)))
    (let lp ((x (car x-range))
             (res '()))
      (if (> x (cdr x-range))
          (make-plot-element (list->vector (reverse res))
                             #f
                             (plot-element-attributes pe))
          (lp (+ x incr)
              (cons ((plot-element-function pe) x) res))))))

(define (plot . o)
  (let lp ((ls o)
           (elements '())
           (attributes '()))
    (cond
     ((null? ls)
      (make-plot (reverse elements) attributes))
     ((symbol? (car ls))
      (if (null? (cdr ls))
          (error "plot attribute without a value" ls)
          (lp (cddr ls) elements (cons (cons (car ls) (cadr ls)) attributes))))
     ((procedure? (car ls))
      (lp (cdr ls) (cons (plot/function (car ls)) elements) attributes))
     ((or (list? (car ls)) (vector? (car ls)) (array? (car ls)))
      (lp (cdr ls) (cons (plot/data (car ls)) elements) attributes))
     ((plot-element? (car ls))
      (lp (cdr ls) (cons (car ls) elements) attributes))
     (else
      (error "unknown argument to plot, expected a function or attribute, got"
             (car ls))))))

(define (write-range name range out)
  (write-string "set " out)
  (write-string name out)
  (write-string " [" out)
  (write (car range) out)
  (write-string ":" out)
  (write (cdr range) out)
  (write-string "]\n" out))

(define (write-array name a out)
  (when name
    (write-string "array " out)
    (write-string name out)
    (write-string "=" out))
  (write-string "[" out)
  ((cond ((vector? a) vector-for-each)
         ((array? a) array-for-each)
         (else for-each))
   (let ((first? #t))
     (lambda (x)
       (if first?
           (set! first? #f)
           (write-string ", " out))
       (write x out)))
   a)
  (write-string "]" out)
  (when name
    (newline out)))

(define (write-style style out)
  (write-string "set style" out)
  (for-each
   (lambda (x) (write-string " " out) (write x out))
   style)
  (newline out))

(define style-defaults
  `((histogram
     (fill solid)
     )))

(define (plot-element-attributes->gnuplot pe)
  (cond
   ((assq 'title: (plot-element-attributes pe))
    => (lambda (cell) `(" title \"" ,(cdr cell) "\"")))
   (else '())))

;; Writes any temporary variables needed for the plot-element and
;; returns a reversed list of the arguments to pass to `plot`.
(define (write-plot-element-data pe gensym out)
  (let ((data (plot-element-data pe))
        (attributes (plot-element-attributes pe)))
    (append
     (reverse (plot-element-attributes->gnuplot pe))
     (cond
      ((or (vector? data)
           (list? data)
           (and (array? data) (= 1 (array-dimension data))))
       (let ((a (gensym "a")))
         (write-array a data out)
         ;; arrays implicitly use the index as column 1 and the value as
         ;; column 2
         `(" using ($2)" ,a)))
      (else
       (let* ((a (gensym "a"))
              (x-col (assq-ref 'x-col: attributes))
              (x-a (and x-col (gensym "x")))
              (label-col (assq-ref 'label-col: attributes))
              (label-a (and label-col (gensym "l")))
              (label-skip (assq-ref 'label-skip: attributes))
              (y-col (assq-ref 'y-col: attributes
                               (cond
                                ((or x-col label-col)
                                 (modulo (+ (max (or x-col 0) (or label-col 0))
                                            1)
                                         (array-dimension data)))
                                (else
                                 0)))))
         (when x-a
           (write-array x-a (array-column data x-col) out))
         (write-array a (array-column data y-col) out)
         (when label-a
           (write-array label-a (array-column data label-col) out))
         `(,@(if label-a
                 (if label-skip
                     (list "[$1]:1/0)" label-a
                           "==0?" (number->string label-skip) ":xtic(int($1)%")
                     (list "[$1])" label-a ":xtic("))
                 '())
           "($2)"
           ,@(if x-a (list "[$1]):" x-a "(") '())
           " using " ,a)))))))

(define (cumulative-elements ls)
  (define (array-map* f a b)
    (array-copy (array-map f a b)))
  (if (null? ls)
      '()
      (let ((ls (reverse ls)))
        (let lp ((ls (cdr ls))
                 (res (list (car ls)))
                 (cum (plot-element-values (car ls))))
          (if (null? ls)
              res
              (let ((vals ((cond ((array? cum) array-map*)
                                 ((vector? cum) vector-map)
                                 (else map))
                           + cum (plot-element-values (car ls)))))
                (lp (cdr ls)
                    (cons (plot-element-with-values (car ls) vals) res)
                    vals)))))))

(define (make-gensym i)
  (lambda (str)
    (let ((res (string-append str (number->string i))))
      (set! i (+ i 1))
      res)))

(define (plot->gnuplot p)
  (define init-plot-cmd '("plot"))
  (define (plot-default-style p)
    'lines)
  (let* ((gensym (make-gensym 0))
         (out (open-output-string))
         (stacked? (assq-ref 'stacked?: (plot-attributes p)))
         (elements (if stacked?
                       (cumulative-elements (plot-elements p))
                       (plot-elements p)))
         (x-range (assq-ref 'x-range: (plot-attributes p)))
         (y-range (or (assq-ref 'y-range: (plot-attributes p))
                      (interval-boundary
                       (map plot-element-interval elements)
                       #t)))
         (width (assq-ref 'width: (plot-attributes p) 600))
         (style (cond ((assq 'style: (plot-attributes p)) => cdr)
                      ((and (pair? elements)
                            (any (lambda (e)
                                   (assq 'style: (plot-element-attributes e)))
                                 elements))
                       => cdr)
                      (else (plot-default-style p)))))
    (when x-range
      (write-range "xrange" x-range out))
    (when y-range
      (write-range "yrange" y-range out))
    (write-string "set style data " out)
    (write style out)
    (newline out)
    (cond
     ((assq style style-defaults)
      => (lambda (attrs)
           (for-each (lambda (style) (write-style style out)) (cdr attrs)))))
    (when stacked?
      (write-style '(data filledcurves above x1) out))
    (cond
     ((assq-ref 'x-label-style: (plot-attributes p))
      => (lambda (style)
           (write-string "set xtics" out)
           (for-each (lambda (x) (write-string " " out) (write x out))
                     style)
           (newline out))))
    (cond
     ((assq-ref 'y-label-style: (plot-attributes p))
      => (lambda (style)
           (write-string "set ytics" out)
           (for-each (lambda (x) (write-string " " out) (write x out))
                     style)
           (newline out))))
    (let lp ((ls elements)
             (plot-cmd init-plot-cmd))
      (cond
       ((null? ls)
        (for-each (lambda (x) (display x out)) (reverse plot-cmd))
        (newline out))
       ((plot-element-function (car ls))
        (lp (cons (plot-function->data (car ls) x-range width)
                  (cdr ls))
            plot-cmd))
       (else
        (lp (cdr ls)
            (append (write-plot-element-data (car ls) gensym out)
                    (if (eq? plot-cmd init-plot-cmd) '(" ") '(", "))
                    plot-cmd)))))
    (get-output-string out)))

(define (process-with-input cmd str)
  (call-with-process-io
   cmd
   (lambda (pid cmd-in cmd-out cmd-err)
     (write-string str cmd-in)
     (close-port cmd-in)
     (let ((res (waitpid pid 0)))
       (unless (zero? (second res))
         (error (port->string cmd-err)))))))

(define (show-plot p)
  (process-with-input '(gnuplot -persist -) (plot->gnuplot p)))

(define (save-plot p file)
  (process-with-input
   '(gnuplot -)
   (let ((format (assq-ref 'format: (plot-attributes p) 'png))
         (width (assq-ref 'width: (plot-attributes p) 600))
         (height (assq-ref 'height: (plot-attributes p) 400)))
     (string-append "set output \"" file "\"\n"
                    "set term " (->string format)
                    " size " (->string width) ", " (->string height) "\n"
                    (plot->gnuplot p)))))
