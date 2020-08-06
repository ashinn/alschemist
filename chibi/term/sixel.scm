
;; ppm image format
(define (write-sixel8 cols rows max-val bv . o)
  (let ((out (if (pair? o) (car o) (current-output-port)))
        (color-map (if (and (pair? o) (pair? (cdr o)))
                       (cadr o)
                       (make-vector 512 #f)))
        (len (bytevector-length bv)))
    (unless (and (> cols 0) (> rows 0))
      (error "cols and rows must be positive"))
    (unless (>= (bytevector-length bv) (* 3 rows cols))
      (error "bytevector of insufficient length for dimensions" bv rows cols))
    ;; enter sixel mode
    (write-string "\x1b;Pq" out)
    ;; write libsixel-style dimension info
    (begin
      (write-string "\"1;1;" out)
      (write cols out)
      (write-char #\; out)
      (write rows out))
    ;; loop over rows
    (let lp ((i 0))
      (cond
       ((>= i len)
        ;; exit sixel mode
        (write-string "\x1b;\\" out))
       (else
        (let* ((end (min len (+ i (* cols 6 3))))
               (colors (bytevector-distinct-colors bv i end)))
          ;; write one color at a time
          ;; TODO: precompute the full color-map with dithering to
          ;; avoid changing colors (which breaks on the tmux sixel
          ;; branch)
          (let lp ((colors colors))
            (when (pair? colors)
              (let ((color (car colors)))
                (set-color! color-map color max-val out)
                (let ((row (make-vector cols 0)))
                  ;; build a row of data
                  (do ((j i (+ j 3)))
                      ((>= j end))
                    (if (= color (bytevector-color-ref bv j))
                        (let* ((j-row (quotient (- j i) (* 3 cols)))
                               (j-col (quotient (- j i (* j-row 3 cols)) 3))
                               (v (bitwise-ior (vector-ref row j-col)
                                               (expt 2 j-row))))
                          (vector-set! row j-col v))))
                  ;; write the row data w/ rle
                  (let lp ((j 1)
                           (prev (vector-ref row 0))
                           (count 1))
                    (define (rle-encode x count)
                      (cond
                       ((= 1 count)
                        (write-char (integer->char (+ 63 x)) out))
                       (else
                        (write-char #\! out)
                        (write count out)
                        (write-char (integer->char (+ 63 x)) out))))
                    (cond
                     ((>= j cols)
                      (rle-encode prev count))
                     ((= prev (vector-ref row j))
                      (lp (+ j 1) prev (+ count 1)))
                     (else
                      (rle-encode prev count)
                      (lp (+ j 1) (vector-ref row j) 1))))
                  (when (pair? (cdr colors))
                    (write-string "$" out))))
              (lp (cdr colors))))
          (if (< end len) (write-string "-" out))
          (lp end)))))))

(define (color->string color max-val)
  (define (p100 n)
    (number->string
     (min 100
          (if (= 100 max-val)
              n
              (exact (round (* 100 (/ n (inexact max-val)))))))))
  (string-append
   (p100 (bitwise-and color #xFF)) ";"
   (p100 (bitwise-and (arithmetic-shift color -8) #xFF)) ";"
   (p100 (bitwise-and (arithmetic-shift color -16) #xFF))))

(define (get-color! color-map color max-val out)
  (let lp ((i 0) (undef #f))
    (cond
     ((>= i (vector-length color-map))
      (let ((reg (or undef 1))  ;; TODO: LRU cache
            (color-rep (color->string color max-val)))
        (vector-set! color-map reg color)
        (write-string "#" out)
        (write-string reg out)
        (write-string ";2;" out)
        (write-string color-rep out)
        reg))
     ((not (vector-ref color-map i))
      (lp (+ i 1) (or undef i)))
     ((= color (vector-ref color-map i))
      i)
     (else
      (lp (+ i 1) undef)))))

(define (set-color! color-map color max-val out)
  (let ((color-reg (get-color! color-map color max-val out)))
    (write-string "#" out)
    (write color-reg out)
    color-reg))

(define (bytevector-color-ref bv i)
  (+ (bytevector-u8-ref bv (+ i 0))
     (arithmetic-shift (bytevector-u8-ref bv (+ i 1)) 8)
     (arithmetic-shift (bytevector-u8-ref bv (+ i 2)) 16)))

;; returns a list of all distinct u8 values in a bytevector,
;; in order of appearance
(define (bytevector-distinct-u8 bv i end)
  (let lp ((j i) (res '()))
    (cond
     ((>= j end) (reverse res))
     ((memv (bytevector-u8-ref bv j) res) (lp (+ j 1) res))
     (else (lp (+ j 1) (cons (bytevector-u8-ref bv j) res))))))

(define (bytevector-distinct-colors bv i end)
  (let lp ((j i) (res '()))
    (if (>= (+ j 2) end)
        (reverse res)
        (let ((color (bytevector-color-ref bv j)))
          (lp (+ j 3) (if (memv color res) res (cons color res)))))))

;; P3 - ascii
(define (read-ppm-data cols rows max-val in)
  (let ((res (make-bytevector (* cols rows 3) 0)))
    (let lp1 ((i 0))
      (let ((line (read-line in)))
        (cond
         ((eof-object? line)
          (if (< i (bytevector-length res))
              (error "premature end of ppm data")
              (values cols rows max-val res)))
         ((or (equal? "" line) (eqv? #\# (string-ref line 0)))
          (lp1 i))
         (else
          (let ((start (string-cursor-start line))
                (end (string-index line #\#)))
            (let lp2 ((ls (map string->number
                               (string-split
                                (substring/cursors line start end)
                                " ")))
                      (i i))
              (cond
               ((null? ls) (lp1 i))
               ((exact-integer? (car ls))
                (if (> (car ls) max-val)
                    (error "maximum ppm value exceeded" (car ls))
                    (bytevector-u8-set! res i (car ls)))
                (lp2 (cdr ls) (+ i 1)))
               (else (lp2 (cdr ls) i)))))))))))

(define (read-ppm in)
  (let ((line (read-line in)))
    (cond
     ((eof-object? line)
      (error "no P3 data"))
     ((or (equal? "P3" line) (equal? "" line) (eqv? #\# (string-ref line 0)))
      (read-ppm in))
     (else
      (let ((cols+rows (map string->number (string-split line " "))))
        (if (not (and (= 2 (length cols+rows))
                      (exact-integer? (car cols+rows))
                      (exact-integer? (cadr cols+rows))
                      (positive? (car cols+rows))
                      (positive? (cadr cols+rows))))
            (error "invalid width or height in P3 header" line)
            (let ((cols (car cols+rows))
                  (rows (cadr cols+rows))
                  (x (read-line in)))
              (if (eof-object? x)
                  (error "premature end of P3 header")
                  (let ((max-val (string->number x)))
                    (if (not (and (exact-integer? max-val)
                                  (positive? max-val)))
                        (error "invalid max value in P3 header" x)
                        (read-ppm-data cols rows max-val in)))))))))))

(define (read-ppm-file file)
  (call-with-input-file file read-ppm))
