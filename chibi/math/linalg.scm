;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general array manipulation

(define (dimensions-compatible? a-domain b-domain axis)
  (and (= (interval-dimension a-domain) (interval-dimension b-domain))
       (let lp ((d (- (interval-dimension a-domain) 1)))
         (or (negative? d)
             (and (or (= d axis)
                      (= (- (interval-upper-bound a-domain d)
                            (interval-lower-bound a-domain d))
                         (- (interval-upper-bound b-domain d)
                            (interval-lower-bound b-domain d))))
                  (lp (- d 1)))))))

(define (interval-width iv axis)
  (- (interval-upper-bound iv axis)
     (interval-lower-bound iv axis)))

(define (interval-widths iv)
  (vector-map -
              (interval-upper-bounds->vector iv)
              (interval-lower-bounds->vector iv)))

;;> Returns a new specialized array which is the concatenatation of 1
;;> or more arrays along the \var{axis} dimension.  All arrays must
;;> have the same dimension, and all domains must have the same width
;;> for each dimension except \var{axis}.
(define (array-concatenate axis a . o)
  (assert (exact-integer? axis)
          (array? a)
          (< -1 axis (array-dimension a))
          (every array? o))
  (let ((a-domain (array-domain a)))
    (assert (every (lambda (b)
                     (dimensions-compatible? a-domain (array-domain b) axis))
                   o))
    (let* ((a-lo (interval-lower-bounds->vector a-domain))
           (c-lo (make-vector (interval-dimension a-domain) 0))
           (c-hi (interval-widths a-domain)))
      (vector-set! c-hi
                   axis
                   (fold (lambda (b sum)
                           (+ sum (interval-width (array-domain b) axis)))
                         (vector-ref c-hi axis)
                         o))
      (let* ((c-domain (make-interval c-lo c-hi))
             (c (make-specialized-array c-domain (array-storage-class a)))
             (b-trans (make-vector (array-dimension a) 0)))
        (array-assign!
         (array-extract c (make-interval c-lo (interval-widths a-domain)))
         (array-translate a (vector-map - a-lo)))
        (let lp ((arrays o)
                 (b-offset (- (interval-upper-bound a-domain axis)
                              (interval-lower-bound a-domain axis))))
          (if (null? arrays)
              c
              (let* ((b (car arrays))
                     (b-domain (array-domain b))
                     (b-offset2 (+ b-offset (interval-width b-domain axis)))
                     (b-lo (make-vector (interval-dimension b-domain) 0))
                     (b-hi (interval-widths b-domain)))
                (vector-set! b-lo axis b-offset)
                (vector-set! b-hi axis b-offset2)
                (vector-set! b-trans axis (- b-offset))
                (let ((view (array-translate
                             (array-extract c (make-interval b-lo b-hi))
                             b-trans)))
                  (array-assign! view b)
                  (lp (cdr arrays) b-offset2)))))))))

;;> Shorthand for array-permute, swapping two dimensions.
(define (array-transpose a . opt)
  (let-optionals opt ((dim1 0) (dim2 1))
    (let ((perm (list->vector (iota (array-dimension a)))))
      (vector-set! perm dim1 dim2)
      (vector-set! perm dim2 dim1)
      (array-permute a perm))))

;;> Returns true iff all arrays have the same domain, and all elements
;;> in the same indexes are \code{=}.
(define (array= a . arrays)
  (and (every (lambda (b) (interval= (array-domain a) (array-domain b)))
              arrays)
       (apply array-every = a arrays)))

;; TODO:
;; array-stack
;; array-rotate-90

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linear algebra

;;> Returns the 2-d identity matrix of size \var{n}.
(define (identity-array n . o)
  (let ((res (make-specialized-array
              (make-interval (vector n n))
              (if (pair? o) (car o) generic-storage-class))))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (do ((j 0 (+ j 1)))
          ((= j n))
        (array-set! res 0 i j)))
    (do ((i 0 (+ i 1)))
        ((= i n) res)
      (array-set! res 1 i i))))

;; ultra-slow leibniz formula
;; (define (determinant a)
;;   (let* ((start (interval-lower-bounds->list (array-domain a)))
;;          (end (interval-lower-bounds->list (array-domain a)))
;;          (i-off (car start))
;;          (j-off (cadr start))
;;          (n (- (car end) i-off))
;;          (nums (iota n))
;;          (det 0)
;;          (sign +))
;;     (permutations-for-each
;;      (lambda (sigma)
;;        (set! det (sign det
;;                        (apply * (map (lambda (i j)
;;                                        (array-ref a (+ i i-off) (+ j j-off)))
;;                                      nums sigma))))
;;        (set! sign (if (eq? sign +) - +)))
;;      nums)
;;     det))

;;> Performs Gaussian elimination and returns the factor to apply to
;;> the determinant.
(define (array-row-echelon! a)
  (assert (array? a) (= (array-dimension a) 2))
  (let* ((domain (array-domain a))
         (n (- (interval-upper-bound domain 0)
               (interval-lower-bound domain 0)))
         (m (- (interval-upper-bound domain 1)
               (interval-lower-bound domain 1))))
    (define (row-swap! i j)
      (do ((k 0 (+ k 1)))
          ((= k m))
        (let ((temp (array-ref a i k)))
          (array-set! a (array-ref a j k) i k)
          (array-set! a temp j k))))
    (define (row-sub! i j factor)
      (do ((k 0 (+ k 1)))
          ((= k m))
        (array-set! a (- (array-ref a i k) (* factor (array-ref a j k))) i k)))
    (let lp ((i 0) (factor 1))
      (cond
        ((= i n) factor)
        ((zero? (array-ref a i i))
         ;; pivot non-zero row to top
         (let lp2 ((j (+ i 1)))
           (cond ((= j n) 0)
                 ((zero? (array-ref a j i))
                  (lp2 (+ j 1)))
                 (else
                  (row-swap! j i)
                  (lp i (* factor -1))))))
        (else
         ;; eliminate other non-zero rows
         (let lp2 ((j (+ i 1)))
           (cond ((= j n) (lp (+ i 1) factor))
                 ((zero? (array-ref a j i))
                  (lp2 (+ j 1)))
                 (else
                  (let ((factor (/ (array-ref a j i)
                                   (array-ref a i i))))
                    (row-sub! j i factor)
                    (lp2 (+ j 1)))))))))))

(define (array-solve-left-identity! a)
  (assert (array? a) (= (array-dimension a) 2))
  (array-row-echelon! a)
  (let* ((n (- (interval-upper-bound (array-domain a) 0)
               (interval-lower-bound (array-domain a) 0)))
         (m (* n 2)))
    ;; zero-out
    (do ((i (- n 1) (- i 1)))
        ((< i 0))
      (let ((divisor (array-ref a i i)))
        (unless (zero? divisor)
          (do ((j (- i 1) (- j 1)))
              ((< j 0))
            (let ((x (/ (array-ref a j i) divisor)))
              (array-set! a 0 i j)
              (do ((k j (+ k 1)))
                  ((= k m))
                (array-set! a (- (array-ref a j k)
                                 (* x (array-ref a i k)))
                            j k)))))))
    ;; reduce
    (do ((i 0 (+ i 1)))
        ((= i n))
      (let ((divisor (array-ref a i i)))
        (unless (zero? divisor)
          (array-set! a 1 i i)
          (do ((j n (+ j 1)))
              ((= j m))
            (array-set! a (/ (array-ref a i j) divisor) i j)))))))

;;> Returns the inverse of 2-d array \var{a}, or \code{#f} if \var{a}
;;> is not invertible.
(define (array-inverse a)
  (assert (array? a) (= (array-dimension a) 2))
  (let* ((domain (array-domain a))
         (n (- (interval-upper-bound domain 0)
               (interval-lower-bound domain 0)))
         (m (- (interval-upper-bound domain 1)
               (interval-lower-bound domain 1))))
    ;; can only compute inverses of square matrices
    (assert (= n m))
    (let* ((id (identity-array n (array-storage-class a)))
           (tmp (array-concatenate 1 a id)))
      (array-solve-left-identity! tmp)
      (and (= 1 (array-ref tmp
                           (- (interval-upper-bound domain 0) 1)
                           (- (interval-upper-bound domain 1) 1)))
           (array-translate
            (array-extract
             tmp
             (make-interval (vector (interval-lower-bound domain 0)
                                    m)
                            (vector (interval-upper-bound domain 0)
                                    (* m 2))))
            (vector 0 (- m)))))))

;;> Computes the determinant of 2-d array \var{a}, mutating \var{a} in
;;> the process.
(define (determinant! a)
   ;; TODO: add determinant for the 2x2x2 case
  (assert (array? a)
          (= (array-dimension a) 2))
  (let* ((domain (array-domain a))
         (hi0 (interval-upper-bound domain 0))
         (lo0 (interval-lower-bound domain 0))
         (hi1 (interval-upper-bound domain 1))
         (lo1 (interval-lower-bound domain 1)))
    (assert (= (- hi0 lo0) (- hi1 lo1)))
    (let ((factor (array-row-echelon! a)))
      (fold (lambda (i acc) (* (array-ref a (+ lo0 i) (+ lo1 i)) acc))
            factor
            (iota (- hi0 lo0))))))

;;> Computes the determinant of 2-d array \var{a}.
(define (determinant a)
  (determinant! (array-copy a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matrix arithmetic

;; Computes \var{alpha} * \var{a}\var{b} + \var{beta} * \var{c},
;; storing the result in \var{c}, for scalars \var{alpha}, \var{beta}
;; and arrays \var{a}, \var{b}, \var{c}.  If \var{beta} is 0 and
;; \var{alpha} 1, then this is equivalent to storing the result of
;; \scheme{(array-mul a b)} into \var{c}.
(define (general-array-mul! alpha a b beta c)
  (assert (and (array? a) (array? b) (array? c)))
  (assert (= 2 (array-dimension a) (array-dimension b) (array-dimension c)))
  (let* ((a-lo (interval-lower-bounds->vector (array-domain a)))
         (a-hi (interval-upper-bounds->vector (array-domain a)))
         (b-lo (interval-lower-bounds->vector (array-domain b)))
         (b-hi (interval-upper-bounds->vector (array-domain b)))
         (n (vector-ref a-hi 0))
         (m (vector-ref a-hi 1))
         (p (vector-ref b-hi 1))
         (off (- (vector-ref b-lo 1) (vector-ref a-lo 1)))
         (a-getter (array-getter a))
         (b-getter (array-getter b))
         (getter (array-getter c))
         (setter (array-setter c)))
    (assert (= (- m (vector-ref a-lo 1))
               (- (vector-ref b-hi 0) (vector-ref b-lo 0))))
    (assert (and (zero? (interval-lower-bound (array-domain c) 0))
                 (zero? (interval-lower-bound (array-domain c) 1))))
    (assert (and (= n (interval-upper-bound (array-domain c) 0))
                 (= p (interval-upper-bound (array-domain c) 1))))
    (do ((i (vector-ref a-lo 0) (+ i 1)))
        ((= i n) c)
      (do ((k (vector-ref b-lo 0) (+ k 1)))
          ((= k p))
        (do ((j (vector-ref a-lo 1) (+ j 1))
             (tmp 0 (+ tmp (* (a-getter i j)
                              (b-getter (+ j off) k)))))
            ((= j m)
             (setter (+ (* beta (getter i k)) (* alpha tmp)) i k)))))))

;; Returns a new array representing the matrix multiplication of 2-d
;; arrays \var{a} and \var{b}.
(define (general-array-mul2 a b) ; NxM * MxP => NxP
  ;; TODO: optimal n-ary mul
  (let* ((a-lo (interval-lower-bounds->vector (array-domain a)))
         (a-hi (interval-upper-bounds->vector (array-domain a)))
         (b-lo (interval-lower-bounds->vector (array-domain b)))
         (b-hi (interval-upper-bounds->vector (array-domain b)))
         (n (vector-ref a-hi 0))
         (m (vector-ref a-hi 1))
         (p (vector-ref b-hi 1))
         (b-off (- (vector-ref b-lo 1) (vector-ref a-lo 1)))
         (off0 (- (vector-ref a-lo 0)))
         (off1 (- (vector-ref a-lo 1)))
         (res (make-specialized-array
               (make-interval (vector (- n (vector-ref a-lo 0))
                                      (- p (vector-ref b-lo 0))))
               (array-storage-class a)))
         (setter (array-setter res)))
    (assert (= (- m (vector-ref a-lo 1))
               (- (vector-ref b-hi 0) (vector-ref b-lo 0))))
    (do ((i (vector-ref a-lo 0) (+ i 1)))
        ((= i n) res)
      (do ((k (vector-ref b-lo 0) (+ k 1)))
          ((= k p))
        (do ((j (vector-ref a-lo 1) (+ j 1))
             (tmp 0 (+ tmp (* (array-ref a i j)
                              (array-ref b (+ j b-off) k)))))
            ((= j m)
             (setter tmp (+ i off0) (+ k off1))))))))

;; convert a matrix multiplication chain to the flattened vector of
;; dimensions for convenient cost lookup
(define (array-mul-chain-dims array-vec)
  (define (array-height array)
    (- (interval-upper-bound (array-domain array) 0)
       (interval-lower-bound (array-domain array) 0)))
  (define (array-width array)
    (- (interval-upper-bound (array-domain array) 1)
       (interval-lower-bound (array-domain array) 1)))
  (let ((res (make-vector (+ (vector-length array-vec) 1))))
    (vector-set! res 0 (array-height (vector-ref array-vec 0)))
    (do ((i 1 (+ i 1)))
        ((= i (vector-length res)) res)
      (let ((array (vector-ref array-vec (- i 1))))
        (assert (= (vector-ref res (- i 1)) (array-height array)))
        (vector-set! res i (array-width array))))))

;; returns the 2D array of lowest cost splits
(define (array-mul-associate array-vec)
  (let* ((num-arrays (vector-length array-vec))
         (dims (array-mul-chain-dims array-vec))
         (costs (make-specialized-array
                 (make-interval (vector num-arrays num-arrays))))
         (get-cost (array-getter costs))
         (set-cost! (array-setter costs))
         (splits (make-specialized-array
                  (make-interval (vector num-arrays num-arrays))))
         (set-split! (array-setter splits)))
    (do ((i 0 (+ i 1)))
        ((= i num-arrays))
      (set-cost! 0 i i))
    (do ((len 2 (+ len 1)))
        ((> len num-arrays)
         splits)
      (do ((i 0 (+ i 1)))
          ((> i (- num-arrays len)))
        (let ((j (+ i len -1)))
          (set-cost! +inf.0 i j)
          (do ((k i (+ k 1)))
              ((= k j))
            (let ((cost (+ (get-cost i k)
                           (get-cost (+ k 1) j)
                           (* (vector-ref dims i)
                              (vector-ref dims (+ k 1))
                              (vector-ref dims (+ j 1))))))
              (when (< cost (get-cost i j))
                (set-cost! cost i j)
                (set-split! k i j)))))))))

;;> Chained matrix multiplication.  For a single array, returns that
;;> array.  For two arrays, performs normal matrix multiplication.
;;> For more arrays, determines the optimal associativity and performs
;;> the corresponding set of multiplications.
(define (array-mul a . o)
  (cond
   ((null? o)
    a)
   ((null? (cdr o))
    (array-mul2 a (car o)))
   (else
    (let* ((vec (list->vector (cons a o)))
           (splits (array-mul-associate vec))
           (get-split (array-getter splits)))
      (let mul ((i 0) (j (- (vector-length vec) 1)))
        (if (= i j)
            (vector-ref vec i)
            (let ((split (get-split i j)))
              (array-mul2 (mul i split) (mul (+ split 1) j)))))))))

;;> Returns \var{a} multiplied by itself \var{pow} times.
(define (array-expt a pow)
  (let loop ((a a) (n pow))
    (case n
      ((0) (identity-array (array-dimension a)))
      ((1) a)
      ((2) (array-mul a a))
      ((3) (array-mul (array-mul a a) a))
      (else
       (let* ((res1 (loop a (arithmetic-shift n -1)))
              (res (array-mul res1 res1)))
         (if (odd? n)
             (array-mul res a)
             res))))))

(define (array-div-left a b)
  (array-mul (array-inverse b) a))

(define (array-div-right a b)
  (array-mul a (array-inverse b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; element-wise operations

(define-syntax define-array-elements-op
  (syntax-rules ()
    ((define-array-elements-op name op)
     (define (name a . o)
       (assert (mutable-array? a))
       (let lp ((ls o))
         (cond
          ((null? ls) a)
          ((array? (car ls))
           (assert (interval= (array-domain a) (array-domain (car ls))))
           (let ((a-getter (array-getter a))
                 (a-setter (array-setter a))
                 (b-getter (array-getter (car ls))))
             (interval-for-each
              (case (array-dimension a)
                ((1)
                 (lambda (i) (a-setter (op (a-getter i) (b-getter i)) i)))
                ((2)
                 (lambda (i j) (a-setter (op (a-getter i j) (b-getter i j)) i j)))
                (else
                 (lambda multi-index
                   (apply a-setter
                          (op (apply a-getter multi-index)
                              (apply b-getter multi-index))
                          multi-index))))
              (array-domain a))
             (lp (cdr ls))))
          (else
           (assert (number? (car ls)))
           (let ((a-getter (array-getter a))
                 (a-setter (array-setter a)))
             (interval-for-each
              (case (array-dimension a)
                ((1)
                 (lambda (i) (a-setter (op (a-getter i) (car ls)) i)))
                ((2)
                 (lambda (i j) (a-setter (op (a-getter i j) (car ls)) i j)))
                (else
                 (lambda multi-index
                   (apply a-setter
                          (op (apply a-getter multi-index) (car ls))
                          multi-index))))
              (array-domain a))
             (lp (cdr ls))))))))))

(define-array-elements-op array-add-elements! +)
(define-array-elements-op array-sub-elements! -)
(define-array-elements-op array-mul-elements! *)
(define-array-elements-op array-div-elements! /)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

;;> Nicely formatted printing for arrays of any rank.  Ranks
;;> higher than 2 are represented as successive 2D drawings.
;;>
;;> Can generate box drawings with left:, right:, etc. keywords
;;> specifying the strings to use.  For example, Jacal-style
;;> matrix output can be done with:
;;>
;;>  (pretty-print-array a #t 'left: "[" 'right: "]")
;;>
;;> and a tic-tac-toe board can be printed with
;;>
;;>  (pretty-print-array a #t 'line-char: "|"
;;>     'top: "-" 'middle-col: "|" center-row: "-")

(define (pretty-print-array a . opt)
  (assert (array? a) (>= (array-dimension a) 2))
  (let* ((p1 (if (pair? opt) (car opt) (current-output-port)))
         (p (cond ((port? p1) p1)
                  (p1 (current-output-port))
                  (else (open-output-string))))
         (rank (array-dimension a))
         (domain (array-domain a))
         (ncols (- (interval-upper-bound domain (- rank 1))
                   (interval-lower-bound domain (- rank 1)))))
    (let-keywords* (if (pair? opt) (cdr opt) '())
        ((fill? #t)
         (line-char #f)
         (left line-char)
         (right left)
         (middle-col #\space)
         (top line-char)
         (bottom top)
         (center-row #f)
         (pad-char #\space))
      (define (replicate x width)
        (if (char? x)
            (make-string width x)
            (let ((len (string-length x)))
              (if (eq? len 1)
                  (make-string width (string-ref x 0))
                  (let ((quot (quotient width len))
                        (rem (remainder width len)))
                    (string-append
                     (apply string-append (map (lambda (n) x) (iota quot)))
                     (substring x 0 rem)))))))
      (define (x->string x)
        (cond
         ((string? x) x)
         ((char? x) (string x))
         (else
          (let ((out (open-output-string)))
            (write x out)
            (get-output-string out)))))
      (define (pad x width)
        (let* ((res (x->string x))
               (len (string-length res)))
          (if (< len width)
              (string-append (make-string (- width len) pad-char) res )
              res)))
      (define (print-width x)
        (cond ((char? x) 1)
              ((string? x) (string-length x))
              (else (string-length (x->string x)))))
      (let ((fixed-width (+ (if left (print-width left) 0)
                            (if right (print-width right) 0)
                            (if middle-col
                                (* (- ncols 1) (print-width middle-col))
                                0))))
        (define (column-widths a)
          (let* ((domain (array-domain a))
                 (res (make-vector (- (interval-upper-bound domain 1)
                                      (interval-lower-bound domain 1))
                                   0)))
            (interval-for-each
             (lambda (row col)
               (vector-set! res col
                            (max (vector-ref res col)
                                 (print-width (array-ref a row col)))))
             domain)
            (vector->list res)))
        (define (print2d a)
          (let* ((domain (array-domain a))
                 (row-start (interval-lower-bound domain 0))
                 (row-end (interval-upper-bound domain 0))
                 (col-start (interval-lower-bound domain 1))
                 (col-end (interval-upper-bound domain 1))
                 (widths (if fill? (column-widths a) '()))
                 (total-width (apply + fixed-width widths)))
            (when top
              (display (if fill? (replicate top total-width) top) p)
              (newline p))
            (let loop1 ((row row-start))
              (when (< row row-end)
                (when left (display left p))
                (let loop2 ((col col-start)
                            (w widths))
                  (when (< col col-end)
                    (display (if (pair? w)
                                 (pad (array-ref a row col) (car w))
                                 (array-ref a row col))
                             p)
                    (when (and middle-col (< (+ col 1) col-end))
                      (display middle-col p))
                    (loop2 (+ col 1) (if (pair? w) (cdr w) w))))
                (when right (display right p))
                (newline p)
                (when (and center-row (< (+ row 1) row-end))
                  (display (if fill? (replicate bottom total-width) bottom))
                  (newline p))
                (loop1 (+ row 1))))
            (when bottom
              (display (if fill? (replicate bottom total-width) bottom) p)
              (newline p))))
        (case rank
          ((0)
           (error "empty array"))
          ((1)
           (if left (display left p))
           (interval-for-each
            (lambda (col)
              (display (x->string (array-ref a col)) p)
              (when (and middle-col
                         (< (+ col 1) (interval-upper-bound domain 0)))
                (display middle-col p)))
            domain))
          ((2)
           (print2d a))
          (else
           (array-for-each
            (lambda (sub) (print2d sub) (newline p))
            (array-curry a (- rank 2)))))
        (if (not p1) (get-output-string p))))))