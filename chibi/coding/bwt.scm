
(define (block-sort str)
  (let* ((s (string-append "\x02;" str "\x03;"))
         (table (sort
                 (map (lambda (i)
                        (string-append (substring s i)
                                       (substring s 0 i)))
                      (iota (string-length s))))))
    (list->string
     (map (lambda (s) (string-ref s (- (string-length s) 1)))
          table))))

(define (invert-block-sort str)
  (let ((len (string-length str))
        (ls (string->list str)))
    (let lp ((i 0)
             (table (make-list len "")))
      (if (>= i len)
          (let ((res (find (lambda (s) (string-suffix? "\x03;" s)) table)))
            (string-remove (lambda (ch) (eqv? ch #\x02))
                           (substring res 0 (- (string-length res) 1))))
          (lp (+ i 1)
              (sort (map (lambda (ch acc) (string-append (string ch) acc))
                         ls
                         table))))))) 
