
;; weighted tree... leaf is (value . weight), node is (left right . weight)

(define (huff-leaf? obj) (and (pair? obj) (not (pair? (cdr obj)))))
(define huff-weight (lambda (x) (if (huff-leaf? x) (cdr x) (cddr x))))
(define huff-value car)
(define huff-left car)
(define huff-right cadr)

(define (huff-node a b weight) (cons a (cons b weight)))

(define (make-huffman-stats obj)
  (let ((table (make-hash-table equal?)))
    ((cond ((vector? obj) vector-for-each)
           ((string? obj) string-for-each)
           (else for-each))
     (lambda (x)
       (hash-table-set! table x (+ 1 (hash-table-ref/default table x 0))))
     obj)
    (hash-table->alist table)))

(define (merge-stats a b)
  (let ((sum (+ (huff-weight a) (huff-weight b))))
    (huff-node a b sum)))

;; collect stats into a weighted tree format
(define (make-huffman-tree lst)
  (if (null? (cdr lst))
      (car lst)
      (make-huffman-tree
       (list-merge (lambda (a b) (< (huff-weight a)
                                (huff-weight b)))
                   (list (merge-stats (car lst) (cadr lst)))
                   (cddr lst)))))

;; build a hash table mapping values to huffman codes
(define (make-huffman-table tree)
  (define table (make-hash-table equal?))
  (define path->huff-code reverse)
  (let lp ((node tree) (path '()))
    (cond
     ((null? node) #f)
     ((huff-leaf? node)
      (hash-table-set! table (huff-value node) (path->huff-code path)))
     (else
      (lp (huff-left node) (cons 0 path))
      (lp (huff-right node) (cons 1 path)))))
  table)

(define (print-path p)
  (cond ((not (null? p))
         (display (car p))
         (print-path (cdr p)))))

(define (path-prefix? p1 p2)
  (cond ((null? p1)
         #t)
        ((null? p2)
         #f)
        ((eq? (car p1) (car p2))
         (path-prefix? (cdr p1) (cdr p2)))
        (else
         #f)))

(define (print-huffman-code table)
  (hash-table-walk
   table
   (lambda (k v)
     (write k)
     (display ": ")
     (print-path v)
     (newline))))

(define (validate-huffman-code table)
  (define (bad-paths p1 p2)
    (error "found a prefix" p1 p2))
  (let outer ((codes (hash-table->alist table)))
    (if (not (null? codes))
        (let ((path1 (cdar codes)))
          (let inner ((rest (cdr codes)))
            (if (not (null? rest))
                (let ((path2 (cdar rest)))
                  (if (path-prefix? path1 path2)
                      (bad-paths path1 path2))
                  (if (path-prefix? path2 path1)
                      (bad-paths path2 path1))
                  (inner (cdr rest))))))
        (outer (cdr codes)))))

;; zero pad
(define (bitlist->u8vector lst)
  (let* ((out (open-output-bytevector))
         (len (length lst))
         (rem (modulo len 8)))
    (let lp ((l (if (= rem 0) lst (append lst (make-list (- 8 rem) 0)))))
      (cond
       ((null? l)
        (get-output-bytevector out))
       (else
        (let build-char ((i 0) (c 0) (bit l))
          (cond
           ((= i 8)
            (write-u8 c out)
            (lp bit))
           (else
            (build-char (+ i 1) (+ (* c 2) (car bit)) (cdr bit))))))))))

(define (u8->bitlist x)
  (list (min 1 (bitwise-and x #b10000000))
        (min 1 (bitwise-and x #b01000000))
        (min 1 (bitwise-and x #b00100000))
        (min 1 (bitwise-and x #b00010000))
        (min 1 (bitwise-and x #b00001000))
        (min 1 (bitwise-and x #b00000100))
        (min 1 (bitwise-and x #b00000010))
        (bitwise-and x #b00000001)))

(define (string->bitlist str)
  (let ((in (open-input-bytevector (string->utf8 str))))
    (let lp ((res '()))
      (let ((c (read-u8 in)))
        (if (eof-object? c)
            (concatenate (reverse res))
            (lp (cons (u8->bitlist c) res)))))))

(define (u8vector->bitlist bv)
  (let ((len (bytevector-length bv)))
    (let lp ((i 0) (res '()))
      (if (>= i len)
          (concatenate (reverse res))
          (lp (+ i 1) (cons (u8->bitlist (bytevector-u8-ref bv i)) res))))))

(define (x->bitlist x)
  (cond ((string? x) (string->bitlist x))
        ((vector? x) (vector->list x))
        ((bytevector? x) (u8vector->bitlist x))
        ((pair? x) x)
        (else (error "can't convert to bitlist " x))))

(define (make-huffman-code stats . opt)
  (define (make-sentinel)
    (list 'EOF))
  (let* ((sentinel (if (pair? opt) (car opt) (make-sentinel)))
         (min-stat (apply min (cons 1 (map cdr stats))))
         (tree (make-huffman-tree (cons (cons sentinel min-stat) stats)))
         (table (make-huffman-table tree))
         (end-code (hash-table-ref table sentinel)))
    ;;(print-huffman-code table)
    (validate-huffman-code table)
    (letrec ((encode-one
              (lambda (obj) (hash-table-ref table obj)))
             (encode
              (lambda (seq)
                (let ((res
                       (fold (lambda (x res) (cons (encode-one x) res)) '()
                             (cond
                              ((vector? seq) (vector->list seq))
                              ((string? seq) (string->list seq))
                              (else seq)))))
                  (bitlist->u8vector
                   (concatenate (reverse (cons end-code res)))))))
             (decode-path
              (lambda (path node)
                (cond ((null? path)
                       (if (huff-leaf? node)
                           (huff-value node)
                           (error "incomplete encoding")))
                      ((huff-leaf? node)
                       (error "invalid encoding"))
                      ((= (car path) 0)
                       (decode-path (cdr path) (huff-left node)))
                      (else
                       (decode-path (cdr path) (huff-right node))))))
             (decode
              (lambda (encoding)
                (let lp ((code (x->bitlist encoding))
                           (node tree)
                           (res '()))
                  (cond
                   ((huff-leaf? node)
                    (let ((v (huff-value node)))
                      (if (equal? v sentinel)
                          (reverse res)
                          (lp code tree (cons (huff-value node) res)))))
                   ((null? code)
                    (reverse res))
                   ((= (car code) 0)
                    (lp (cdr code) (huff-left node) res))
                   (else
                    (lp (cdr code) (huff-right node) res)))))))
      (lambda (command . args)
        (case command
          ;; basic interface
          ((encode)
           (encode (car args)))
          ((decode decode-list)
           (decode (car args)))
          ((decode-string)
           (list->string (decode (car args))))
          ((decode-vector)
           (list->vector (decode (car args))))
          ((encoder) encode)
          ((decoder list-decoder) decode)
          ((string-decoder)
           (lambda (x) (list->string (decode x))))
          ((vector-decoder)
           (lambda (x) (list->vector (decode x))))
          ;; introspection
          ((get-sentinel)   sentinel)
          ((get-table)      table)
          ((get-tree)       tree)
          ((set-sentinel!)  (set! sentinel (car args)))
          ((set-table!)     (set! table (car args)))
          ((set-tree!)      (set! tree (car args)))
          (else
           (error "unknown command " command)))))))

;; convenience procedures

(define (huffman-encode seq . opt-stats)
  (let* ((stats (if (pair? opt-stats)
                    (car opt-stats)
                    (make-huffman-stats seq)))
         (huff (make-huffman-code stats)))
    (list (huff 'encode seq) huff)))

(define (make-huffman-decoder command)
  (lambda (seq stats . opt-sentinel)
    (let ((huff (apply make-huffman-code (append (list stats) opt-sentinel))))
      (list (huff command seq) huff))))

(define huffman-decode (make-huffman-decoder 'decode))
(define huffman-decode-string (make-huffman-decoder 'decode-string))
(define huffman-decode-vector (make-huffman-decoder 'decode-vector))
