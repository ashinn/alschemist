
; tree.scm

(define (tree-display tree . optional)

  ; CHANGE THESE DEFINITIONS TO SUIT YOUR NEEDS... (or better yet, add them
  ; as parameters to tree-display)

  ; how many space characters between trees
  (define tree-spacing 1)

  ; print tree with leaves at bottom?
  (define leaves-at-bottom? #t)

  ; define what a tree is (leaf & internal node) and how to get its components
  (define (leaf? tree)             (= (cadar tree) 0)) ; (not (pair? tree))
  (define (leaf-name tree)         (caar tree))        ; (string->symbol ".")
  (define (leaf-info tree)         (cadr tree))        ; tree
  (define (int-node-name tree)     (caar tree))        ; (string->symbol ".")
  (define (int-node-children tree) (cdr tree))         ; (list (car tree) (cdr tree))

  (define (make-augm-leaf width root name info)
    (vector 'leaf width root name info))

  (define (make-augm-pad width)
    (vector 'pad width))

  (define (make-augm-int-node width root name lpad rpad children)
    (vector #f width root name lpad rpad children))

  (define (augm-tree-int-node? x)    (not (vector-ref x 0)))
  (define (augm-tree-pad? x)         (eq? (vector-ref x 0) 'pad))
  (define (augm-tree-width x)        (vector-ref x 1))
  (define (augm-tree-root x)         (vector-ref x 2))
  (define (augm-tree-name x)         (vector-ref x 3))
  (define (augm-leaf-info x)         (vector-ref x 4))
  (define (augm-int-node-lpad x)     (vector-ref x 4))
  (define (augm-int-node-rpad x)     (vector-ref x 5))
  (define (augm-int-node-children x) (vector-ref x 6))

  (define (pad width l)
    (if (> width 0)
      (cons (make-augm-pad width) l)
      l))

  (define (field-width x) ; return number of chars in the written repr of `x'
    (cond ((boolean? x) 2)
          ((symbol? x)  (string-length (symbol->string x)))
          ((char? x)    (case x ((#\space) 7) ((#\newline) 9) (else 3)))
          ((number? x)  (string-length (number->string x)))
          ((vector? x)  (+ (field-width (vector->list x)) 1))
          ((null? x)    2)
          ((pair? x)    (let loop ((l (cdr x)) (w (+ (field-width (car x)) 2)))
                          (cond ((null? l)
                                 w)
                                ((pair? l)
                                 (loop (cdr l) (+ w (field-width (car l)) 1)))
                                (else
                                 (+ w (field-width l) 3)))))
          ((string? x)  (let loop ((i (- (string-length x) 1)) (w 2))
                          (if (>= i 0)
                            (let ((c (string-ref x i)))
                              (loop (- i 1)
                                    (+ w (case c ((#\\ #\") 2) (else 1)))))
                            w)))
          (else         0)))

  (define (augment-tree tree)
    (if (leaf? tree)

      (let* ((name (leaf-name tree))
             (info (leaf-info tree))
             (name-width (field-width name))
             (info-width (field-width info))
             (tree-width (max name-width info-width)))
        (make-augm-leaf tree-width (quotient tree-width 2) name info))

      (let* ((children (map augment-tree (int-node-children tree)))
             (name (int-node-name tree))
             (name-width (field-width name))
             (name-left (quotient name-width 2))
             (name-right (- name-width name-left)))
        (if (null? children)
          (make-augm-int-node name-width name-left name 0 0 '())
          (let* ((first-child (car children))
                 (last-child (list-ref children (- (length children) 1)))
                 (width
                   (+ (* (- (length children) 1) tree-spacing)
                      (apply + (map augm-tree-width children))))
                 (left
                   (quotient (+ (- width (augm-tree-width last-child))
                                (+ (augm-tree-root first-child)
                                   (augm-tree-root last-child)))
                             2))
                 (right
                   (- width left))
                 (max-left
                   (max name-left left))
                 (max-right
                   (max name-right right)))
            (make-augm-int-node (+ max-left max-right) max-left name
                                (- max-left left) (- max-right right)
                                children))))))

  (define (any-int-nodes? trees)
    (if (null? trees)
      #f
      (or (augm-tree-int-node? (car trees))
          (any-int-nodes? (cdr trees)))))

  (define (all-done? trees)
    (if (null? trees)
      #t
      (and (augm-tree-pad? (car trees))
           (all-done? (cdr trees)))))

  (define (seq c n port)
    (if (> n 0)
      (begin
        (write-char c port)
        (seq c (- n 1) port))))

  (define (print-trees trees port)
    (if (not (all-done? trees))
      (let ((delay-leaves? (and leaves-at-bottom? (any-int-nodes? trees))))

        (let loop1 ((l trees))
          (if (pair? l)
            (let* ((tree (car l))
                   (tree-width (augm-tree-width tree)))
              (if (augm-tree-pad? tree)
                (begin
                  (seq #\space tree-width port)
                  (loop1 (cdr l)))
                (let* ((root (augm-tree-root tree))
                       (name (augm-tree-name tree))
                       (name-width (field-width name))
                       (name-left (quotient name-width 2))
                       (name-right (- name-width name-left)))
                  (if (or (not delay-leaves?) (augm-tree-int-node? tree))
                    (begin
                      (seq #\space (- root name-left) port)
                      (write name port)
                      (seq #\space (- tree-width root name-right) port)
                      (loop1 (cdr l)))
                    (begin
                      (seq #\space root port)
                      (write-char #\. port)
                      (seq #\space (- tree-width root 1) port)
                      (loop1 (cdr l)))))))))

        (newline port)

        (let loop2 ((l trees) (new-trees '()))
          (if (pair? l)
            (let* ((tree (car l))
                   (tree-width (augm-tree-width tree)))
              (if (augm-tree-pad? tree)
                (begin
                  (seq #\space tree-width port)
                  (loop2 (cdr l) (append new-trees (list tree))))
                (let* ((root (augm-tree-root tree))
                       (name (augm-tree-name tree))
                       (name-width (field-width name))
                       (name-left (quotient name-width 2))
                       (name-right (- name-width name-left)))
                  (if (augm-tree-int-node? tree)
                    (let ((children (augm-int-node-children tree)))
                      (if (null? children)
                        (begin
                          (seq #\space (- root name-left) port)
                          (write name port)
                          (seq #\space (- tree-width root name-right) port)
                          (loop2 (cdr l)
                                 (append new-trees (pad tree-width '()))))
                        (let* ((child1 (car children))
                               (root1 (augm-tree-root child1))
                               (width1 (augm-tree-width child1))
                               (lpad (augm-int-node-lpad tree))
                               (rpad (augm-int-node-rpad tree)))
                          (seq #\space (+ lpad root1) port)
                          (write-char #\. port)
                          (let loop3 ((l1 (cdr children))
                                      (l2 (cons child1 (pad lpad '())))
                                      (right (- width1 (+ root1 1))))
                            (if (pair? l1)
                              (let* ((child (car l1))
                                     (root (augm-tree-root child))
                                     (width (augm-tree-width child)))
                                (seq #\- (+ root tree-spacing right) port)
                                (write-char #\. port)
                                (loop3 (cdr l1)
                                       (cons child (pad tree-spacing l2))
                                       (- width (+ root 1))))
                              (begin
                                (seq #\space (+ right rpad) port)
                                (loop2 (cdr l)
                                       (append new-trees
                                               (reverse (pad rpad l2))))))))))
                    (if delay-leaves?
                      (begin
                        (seq #\space root port)
                        (write-char #\. port)
                        (seq #\space (- tree-width root 1) port)
                        (loop2 (cdr l) (append new-trees (list tree))))
                      (let* ((info (augm-leaf-info tree))
                             (info-width (field-width info))
                             (info-left (quotient info-width 2))
                             (info-right (- info-width info-left)))
                        (seq #\space (- root info-left) port)
                        (write info port)
                        (seq #\space (- tree-width root info-right) port)
                        (loop2 (cdr l)
                               (append new-trees (pad tree-width '())))))))))

            (begin
              (newline port)
              (print-trees new-trees port)))))))

  (print-trees (list (augment-tree tree))
               (if (null? optional) (current-output-port) (car optional))))

