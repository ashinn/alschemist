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

;;> Translates \var{array} so that it's lower bounds are all zero.
(define (array-to-origin array)
  (array-translate
   array
   (vector-map - (interval-lower-bounds->vector (array-domain array)))))

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
  (assert (and (array? a) (every array? arrays)))
  (and (every (lambda (b) (interval= (array-domain a) (array-domain b)))
              arrays)
       (apply array-every = a arrays)))

;;> Returns the first element of a. Throws an exception if a is empty.
(define (array-first a)
  (apply array-ref a (interval-lower-bounds->list (array-domain a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linear algebra

(define (array-of init a . o)
  (let* ((domain (if (array? a) (array-domain a) a))
         (storage
          (cond ((pair? o) (car o))
                ((and (array? a) (specialized-array? a))
                 (array-storage-class a))
                (else generic-storage-class)))
         (val (+ init (or (storage-class-default storage) 0))))
    (make-specialized-array domain storage val)))

;;> Returns an all-zero array with the same domain as \var{a}, either
;;> an array or an interval.  \var{storage} defaults to the storage
;;> class of \var{a} if it's a specialized array, otherwise
;;> generic-storage-class.
(define (zeros a . o)
  (apply array-of 0 a o))

;;> Similar to zeros but fills the array with one, useful as the
;;> multiplicative identify.
(define (ones a . o)
  (apply array-of 1 a o))

;;> Returns the 2-d identity matrix of size \var{n}.
(define (identity-array n . o)
  (let* ((res (apply zeros (make-interval (vector n n)) o))
         (one (+ 1 (array-ref res 0 0))))
    (do ((i 0 (+ i 1)))
        ((= i n) res)
      (array-set! res one i i))))

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
  (assert (and (array? a) (= (array-dimension a) 2)))
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
  (assert (and (array? a) (= (array-dimension a) 2)))
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
  (assert (and (array? a) (= (array-dimension a) 2)))
  (let* ((domain (array-domain a))
         (n (- (interval-upper-bound domain 0)
               (interval-lower-bound domain 0)))
         (m (- (interval-upper-bound domain 1)
               (interval-lower-bound domain 1))))
    ;; can only compute inverses of square matrices
    (assert (= n m))
    (let* ((id (identity-array n (array-storage-class a)))
           (tmp (array-append 1 (list a id))))
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
  (assert (and (array? a)
               (= (array-dimension a) 2)))
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
  (let* ((a-lo (interval-lower-bounds->vector (array-domain a)))
         (a-hi (interval-upper-bounds->vector (array-domain a)))
         (b-lo (interval-lower-bounds->vector (array-domain b)))
         (b-hi (interval-upper-bounds->vector (array-domain b)))
         (n (vector-ref a-hi 0))
         (m (vector-ref a-hi 1))
         (p (vector-ref b-hi 1))
         (b-off (- (vector-ref b-lo 1) (vector-ref a-lo 1)))
         (off0 (- (vector-ref a-lo 0)))
         (off1 (- (vector-ref b-lo 0)))
         (res (make-specialized-array
               (make-interval (vector (- n (vector-ref a-lo 0))
                                      (- p (vector-ref b-lo 0))))
               (or (array-storage-class a) generic-storage-class)))
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

(define (array-height array)
  (- (interval-upper-bound (array-domain array) 0)
     (interval-lower-bound (array-domain array) 0)))
(define (array-width array)
  (- (interval-upper-bound (array-domain array) 1)
     (interval-lower-bound (array-domain array) 1)))

;; convert a matrix multiplication chain to the flattened vector of
;; dimensions for convenient cost lookup
(define (array-mul-chain-dims array-vec)
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
(define (array-mul-expt a pow)
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

(define (list-set ls i v)
  (let ((res (list-copy ls)))
    (list-set! res i v)
    res))

;;> "Broadcasts" the array to the new-domain, repeating single-width
;;> dimensions as needed.  Follows the numpy conventions -
;;> conceptually the domains are right aligned, with the array domain
;;> being wrapped in sufficient single-width outer dimensions to match
;;> the new-domain dimensions.  Then every dimension width is
;;> compared: if they are the same width they are aligned accordingly,
;;> otherwise if the array dimension width is 1, it's values are
;;> repeated for every value of the new-domain width.  Throws an error
;;> if neither of these cases apply.
(define (array-broadcast array new-domain)
  (cond
   ((not (array? array))
    (array-broadcast (list*->array 0 array) new-domain))
   ((equal? (array-domain array) new-domain)
    array)
   (else
    (let* ((domain (array-domain array))
           (array-dim (interval-dimension domain))
           (new-dim (interval-dimension new-domain))
           (drop-dims (- new-dim array-dim)))
      (assert (not (negative? drop-dims)))
      (let lp ((d 1)
               (update (lambda ls ls)))
        (cond
         ((> d array-dim)
          (specialized-array-share
           (if (specialized-array? array) array (array-copy array))
           new-domain
           (lambda multi-index
             (apply values (apply update (drop multi-index drop-dims))))))
         (else
          (let* ((a-dim (- array-dim d))
                 (n-dim (- new-dim d))
                 (lb (interval-lower-bound domain a-dim)))
            (cond
             ((= (interval-width domain a-dim)
                 (interval-width new-domain n-dim))
              (if (= (interval-lower-bound domain a-dim)
                     (interval-lower-bound new-domain n-dim))
                  (lp (+ d 1) update)
                  ;; translate
                  (let ((offset (- (interval-lower-bound new-domain n-dim) lb)))
                    (lp (+ d 1)
                        (lambda multi-index
                          (let ((res (apply update multi-index)))
                            (list-set res a-dim (+ (list-ref res a-dim) offset))
                            ))))))
             ((= 1 (interval-width domain a-dim))
              ;; broadcast
              (lp (+ d 1)
                  (lambda multi-index
                    (list-set (apply update multi-index) a-dim lb))))
             (else
              (error "can't broadcast array to domain at dimension"
                     array new-domain a-dim)))))))))))

(define (array-squeeze a . o)
  (let ((pred (if (pair? o)
                  (cond
                   ((procedure? (car o)) (car o))
                   ((integer? (car o)) (lambda (i) (= i (car o))))
                   (else (lambda (i) (memv i (car o)))))
                  (lambda (i) #t))))
    (call-with-values
        (lambda ()
          (let ((interval (array-domain a)))
            (partition (lambda (k)
                         (and (pred k) (eqv? (interval-width interval k) 1)))
                       (iota (array-dimension a)))))
      (lambda (ones rest)
        (car (array->list
              (array-curry
               (array-permute a (list->vector (append ones rest)))
               (length rest))))))))

;; Defines an elementwise array operation of two arguments,
;; arrays a and b, which applies op to all of the corresponding
;; elements, (op a_i a_j), and stores the result in a, broadcasting
;; where needed.  fast-path is attempted first, and the result of
;; that is used instead if not #f.
;; Note because the result is stored in a, a must already be of the
;; appropriate size, i.e. we only broadcast b dimensions into a, not
;; vice versa.
(define-syntax define-array-elementwise-binary-op
  (syntax-rules ()
    ((define-array-elementwise-binary-op name op fast-path)
     (define (name a b)
       (assert (mutable-array? a))
       (let ((b (array-broadcast b (array-domain a))))
         (or (fast-path a b)
             (let ((a-getter (array-getter a))
                   (a-setter (array-setter a))
                   (b-getter (array-getter b)))
               (case (interval-dimension (array-domain a))
                 ((0)
                  (array-set! a (op (array-ref a) (array-ref b)))
                  a)
                 ((1)
                  (interval-for-each
                   (lambda (i)
                     (a-setter (op (a-getter i) (b-getter i)) i))
                   (array-domain a))
                  a)
                 ((2)
                  (interval-for-each
                   (lambda (i j)
                     (a-setter (op (a-getter i j) (b-getter i j)) i j))
                   (array-domain a))
                  a)
                 ((3)
                  (interval-for-each
                   (lambda (i j k)
                     (a-setter (op (a-getter i j k) (b-getter i j k)) i j k))
                   (array-domain a))
                  a)
                 (else
                  (interval-for-each
                   (lambda multi-index
                     (apply a-setter
                            (op (apply a-getter multi-index)
                                (apply b-getter multi-index))
                            multi-index))
                   (array-domain a))
                  a)))))))))

(define-array-elementwise-binary-op array+! + fast-array+!)
(define-array-elementwise-binary-op array-! - fast-array-!)
(define-array-elementwise-binary-op array*! * fast-array*!)
(define-array-elementwise-binary-op array/! / fast-array/!)

(define-array-elementwise-binary-op array-min! min (lambda (a b) #f))
(define-array-elementwise-binary-op array-max! max (lambda (a b) #f))

;;> Elementwise in-place array operations.  Applies the operator to
;;> each corresponding element of both arrays, and stores the result
;;> in the first array.  Broadcasts the second array (which can be a
;;> scalar) as needed.
;;/

;; Functional arithmetic operations.  Need utilities to generate the
;; destination array.

(define (fit-broadcast-domain a b)
  (let ((a-len (vector-length a))
        (b-len (vector-length b)))
    (let lp ((i 0) (res (vector-copy a)))
      (define (fit m n)
        (cond ((= m n) m)
              ((= m 1) n)
              ((= n 1) m)
              (else (error "domains can't be broadcast" a b i))))
      (cond
       ((>= i a-len)
        (if (>= i b-len)
            res
            (vector-append (vector-copy b 0 (- b-len i)) res)))
       ((>= i b-len)
        res)
       (else
        (vector-set! res
                     (- a-len i 1)
                     (fit (vector-ref res (- a-len i 1))
                          (vector-ref b (- b-len i 1))))
        (lp (+ i 1) res))))))

(define (list-of-arrays->broadcast-domain arrays)
  (let lp ((ls arrays)
           (shape (vector)))
    (cond
     ((null? ls)
      (make-interval shape))
     ((not (array? (car ls)))
      (lp (cdr ls) shape))
     (else
      (lp (cdr ls)
          (fit-broadcast-domain shape
                                (interval-widths (array-domain (car ls)))))))))

(define storage-class-widths
  `((,u1-storage-class . 1)
    (,u8-storage-class . 8)
    (,u16-storage-class . 16)
    (,u32-storage-class . 32)
    (,u64-storage-class . 64)
    (,s8-storage-class . 108)
    (,s16-storage-class . 116)
    (,s32-storage-class . 132)
    (,s64-storage-class . 164)
    (,f8-storage-class . 208)
    (,f16-storage-class . 216)
    (,f32-storage-class . 232)
    (,f64-storage-class . 264)
    (,c64-storage-class . 332)
    (,c128-storage-class . 364)
    ))

(define (scalar-storage-class z)
  (cond ((exact-integer? z) s64-storage-class)
        ((real? z) f32-storage-class)
        ((complex? z) c64-storage-class)
        (else generic-storage-class)))

;; Chooses the smallest storage class capable of holding the elements
;; of both storage classes s1 and s2.
(define (storage-class-fit s1 s2)
  (if (eq? s1 s2)
      s1
      (let ((s1c (assq s1 storage-class-widths))
            (s2c (assq s2 storage-class-widths)))
        (if (and s1c s2c)
            (let-values
                (((s1 s2 s1w s2w)
                  (if (> (cdr s2c) (cdr s1c))
                      (values s2 s1 (cdr s2c) (cdr s1c))
                      (values s1 s2 (cdr s1c) (cdr s2c)))))
              (cond
               ((> s1w 300)
                (if (eq? s2 f64-storage-class)
                    c128-storage-class
                    s1))
               ((> s1w 200)
                (if (or (eq? s2 u64-storage-class) (eq? s2 s64-storage-class))
                    f64-storage-class
                    s1))
               ((> s1w 100)
                (if (>= s2w (- s1w 100))
                    (if (eq? s1 s64-storage-class)
                        generic-storage-class
                        s64-storage-class)
                    s1))
               (else s1)))
            generic-storage-class))))

;; Chooses a storage class capable of holding the elements of all of
;; the arrays.
(define (widest-storage-class arrays)
  (cond
   ((null? arrays) generic-storage-class)
   ((and (array? (car arrays)) (not (specialized-array? (car arrays))))
    generic-storage-class)
   (else
    (let lp ((ls (cdr arrays))
             (storage ((if (array? (car arrays))
                           array-storage-class
                           scalar-storage-class)
                       (car arrays))))
      (cond
       ((null? ls) storage)
       ((not (array? (car ls)))
        (lp (cdr ls)
            (storage-class-fit storage (scalar-storage-class (car ls)))))
       ((not (specialized-array? (car ls))) generic-storage-class)
       (else
        (let ((storage (storage-class-fit storage
                                          (array-storage-class (car ls)))))
          (if (eq? storage generic-storage-class)
              storage
              (lp (cdr ls) storage)))))))))

(define (list-of-arrays->broadcast-dest arrays)
  (let ((domain (list-of-arrays->broadcast-domain arrays))
        (storage (widest-storage-class arrays)))
    (zeros domain storage)))

;; First generate a zero array capable of holding the result, add in
;; the first array then apply the op on the remaining arrays.

(define (unwrap-trivial-from-scalars x arrays)
  (if (and (zero? (array-dimension x)) (not (any array? arrays)))
      (array-ref x)
      x))

(define (array+ . arrays)
  (let ((dest (list-of-arrays->broadcast-dest arrays)))
    (for-each (lambda (array) (array+! dest array)) arrays)
    (unwrap-trivial-from-scalars dest arrays)))
(define (array- . arrays)
  (let ((dest (list-of-arrays->broadcast-dest arrays)))
    (cond
     ((and (pair? arrays) (null? (cdr arrays)))
      (array-! dest (car arrays)))
     (else
      (array+! dest (car arrays))
      (for-each (lambda (array) (array-! dest array)) (cdr arrays))))
    (unwrap-trivial-from-scalars dest arrays)))
(define (array* . arrays)
  (let ((dest (list-of-arrays->broadcast-dest arrays)))
    (array+! dest (car arrays))
    (for-each (lambda (array) (array*! dest array)) (cdr arrays))
    (unwrap-trivial-from-scalars dest arrays)))
(define (array/ . arrays)
  (let ((dest (list-of-arrays->broadcast-dest arrays)))
    (cond
     ((and (pair? arrays) (null? (cdr arrays)))
      (array+! dest 1.0)
      (array/! dest (car arrays)))
     (else
      (array+! dest (car arrays))
      (for-each (lambda (array) (array/! dest array)) (cdr arrays))))
    (unwrap-trivial-from-scalars dest arrays)))

(define (array-min . arrays)
  (let ((dest (list-of-arrays->broadcast-dest arrays)))
    (array+! dest (car arrays))
    (for-each (lambda (array) (array-min! dest array)) (cdr arrays))
    (unwrap-trivial-from-scalars dest arrays)))
(define (array-max . arrays)
  (let ((dest (list-of-arrays->broadcast-dest arrays)))
    (array+! dest (car arrays))
    (for-each (lambda (array) (array-max! dest array)) (cdr arrays))
    (unwrap-trivial-from-scalars dest arrays)))

(define (array-relu array)
  (array-max 0 array))

;; General mapping.

(define (general-array-dot a b)
  (assert (and (array? a) (array? b)))
  (assert (= (array-dimension a) (array-dimension b)))
  (let ((sum 0))
    (array-for-each (lambda (x y) (set! sum (+ sum (* x y)))) a b)
    sum))

(define (array-map-elements! proc a)
  (if (array? a)
      (let ((a-getter (array-getter a))
            (a-setter (array-setter a)))
        (interval-for-each
         (case (array-dimension a)
           ((1)
            (lambda (i) (a-setter (proc (a-getter i)) i)))
           ((2)
            (lambda (i j) (a-setter (proc (a-getter i j)) i j)))
           (else
            (lambda multi-index
              (apply a-setter
                     (proc (apply a-getter multi-index))
                     multi-index))))
         (array-domain a))
        a)
      (proc a)))

(define (array-map-elements proc a)
  (if (array? a) (array-map proc a) (proc a)))

(define (array-exp a) (array-map-elements exp a))
(define (array-exp! a) (array-map-elements! exp a))
(define (array-log a) (array-map-elements log a))
(define (array-log! a) (array-map-elements! log a))

(define (array-expt a x)
  (array-map-elements (lambda (y) (expt y x)) a))
(define (array-expt! a x)
  (array-map-elements! (lambda (y) (expt y x)) a))
(define (array-square a)
  (array-expt a 2))
(define (array-square! a)
  (array-expt! a 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; norms

;;> Returns the sum of all elements in array \var{a}.  Not a norm
;;> because it can yield negative results.
(define (array-sum a)
  (if (array? a) (array-fold-left + 0 a) a))

;;> Returns the sum of the absolute values of all elements in array
;;> \var{a}.  Aka the L1-norm, taxicab norm, or Manhattan norm.
(define (array-1norm a)
  (if (array? a) (array-fold-left (lambda (acc x) (+ (abs x) acc)) 0 a) a))

;;> Returns the sum of the square of all elements in array \var{a}.
;;> Aka the L2-norm, Euclidean norm, Frobenius norm or square norm.
(define (array-2norm a)
  (sqrt (if (array? a) (array-dot a a) (square a))))

;;> Returns the sum of the absolute value of all elements in array
;;> \var{a} raised to the \var{p} power.  Aka the p-norm, this is the
;;> generalized form of the above.
(define (array-norm a p)
  (expt (if (array? a)
            (array-fold-left (lambda (acc x) (+ (expt (abs x) p) acc)) 0 a)
            (expt (abs a) p))
        (/ p)))

;;> Returns the maximum absolute value of all elements in array
;;> \var{a}.  Aka the max norm or infinity norm.
(define (array-inf-norm a)
  (if (array? a) (array-fold-left (lambda (acc x) (max (abs x) acc)) 0 a) a))

(define array-max-norm array-inf-norm)

;;> Returns the average (Euclidean mean) of all elements in the array.
(define (array-mean a)
  (if (array? a) (/ (array-sum a) (interval-volume (array-domain a))) a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

;;> Returns an array of the rows of a as 1-dimensional arrays.
(define (array-rows a)
  (array-curry a (- (array-dimension a) 1)))

;;> Returns an array of the columns of a as 1-dimensional arrays.
(define (array-columns a)
  (array-rows (array-transpose a)))

;;> Returns an array of just the selected column indexes of a per row.
;;> Equivalent to pytorch: a[torch.arange(num), indexes]
(define (array-select-columns a indexes . o)
  (let ((storage (if (pair? o)
                     (car o)
                     (or (array-storage-class a) generic-storage-class))))
    (list*->array
     1
     (map (lambda (i j) (array-ref a i j))
          (iota (length indexes))
          indexes)
     storage)))

;;> The inverse of array-select-columns.  Selects only the given
;;> columns indexes per row, setting all other columns to 0.
(define (array-unselect-columns a indexes . o)
  (let* ((n (interval-width (array-domain a) 0))
         (storage (if (pair? o)
                      (car o)
                      (or (array-storage-class a) generic-storage-class)))
         (res (make-specialized-array (make-interval (vector n n)) storage)))
    (for-each
     (lambda (i j)
       (array-set! res (array-ref a j) j i))
     (iota n)
     indexes)
    res))

;;> Returns a 1-dimensional array of the diagonal elements of a.
(define (array-diag a)
  (let ((get-a (array-getter a)))
    (make-array (make-interval (vector (interval-width (array-domain a) 0)))
                (lambda (i) (get-a i i)))))

;;> Returns an array of the sum of each element of an axis of a,
;;> defaulting to the last.
(define (array-sum-axis a . o)
  (let* ((axis (if (pair? o) (car o) (- (array-dimension a) 1)))
         (widths (let ((w (interval-widths (array-domain a))))
                   (vector-set! w axis 1)
                   w))
         (storage (if (specialized-array? a)
                      (array-storage-class a)
                      generic-storage-class)))
    (array-mul a (ones (make-interval widths) storage))))

;;> Sums the axis and squeezes out the resulting sum axis.
(define (array-sum-axis/squeeze a . o)
  (let ((axis (if (pair? o) (car o) (- (array-dimension a) 1))))
    (array-squeeze (array-sum-axis a axis) axis)))

;;> Returns an array of the sum of each row of a.
(define array-sum-rows array-sum-axis)

;;> Divide elements of each row by their sum.
;;> Equivalent to the pytorch: a / a.sum(dim=1, keepdim=True)
(define (array-normalize-rows a)
  (let ((rows (array-rows a)))
    (array-stack
     0
     (array->list
      (array-map (lambda (row) (array/ row (array-sum row)))
                 rows)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; convolutions

;;> Returns the convolution of array \var{a} using the given
;;> \var{kernel}.
(define (array-convolve a kernel)
  (assert (and (array? a) (array? kernel)))
  (let* ((kernel (array-to-origin kernel))
         (kernel-domain (array-domain kernel))
         (kernel-widths (interval-upper-bounds->vector kernel-domain))
         (domain (array-domain a))
         (res-domain
          (make-interval (interval-lower-bounds->vector domain)
                         (vector-map -
                                     (interval-upper-bounds->vector domain)
                                     kernel-widths
                                     (make-vector (vector-length kernel-widths)
                                                  -1))))
         (res (make-specialized-array res-domain
                                      (or (array-storage-class a)
                                          generic-storage-class)))
         (setter (array-setter res)))
    ;; TODO: flatten this into a single array-mul:
    ;; 1. pre-expand the windows of a into columns
    ;; 2. multiply the kernel (flattened to 1d) * the columns
    ;; 3. reshape the result
    (interval-for-each
     (case (array-dimension a)
       ((1)
        ;; by convention the kernels in 1D convolutions are reversed
        (let ((kernel (array-reverse kernel))
              (kernel-elts (array-height kernel)))
          (lambda (i)
            (let ((end-elt (+ i kernel-elts)))
              (let ((window
                     (array-to-origin
                      (array-extract a (make-interval
                                        (vector i)
                                        (vector end-elt))))))
                (setter (array-dot kernel window) i))))))
       ((2)
        (let ((kernel-rows (array-height kernel))
              (kernel-cols (array-width kernel)))
          (lambda (i j)
            (let ((end-row (+ i kernel-rows))
                  (end-col (+ j kernel-cols)))
              (let ((window
                     (array-to-origin
                      (array-extract a (make-interval
                                        (vector i j)
                                        (vector end-row end-col))))))
                (setter (array-dot kernel window) i j))))))
       (else
        (let ((kernel-offs
               (interval-upper-bounds->vector (array-domain kernel))))
          (lambda multi-index
            (let* ((lower (list->vector multi-index))
                   (upper (vector-map + lower kernel-offs)))
              (let ((window
                     (array-to-origin
                      (array-extract a (make-interval lower upper)))))
                (apply setter (array-dot kernel window) multi-index)))))))
     res-domain)
    res))

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
;;>     'top: "-" 'middle-col: "|" 'center-row: "-")

(define (pretty-print-array a . opt)
  (assert (and (array? a) (>= (array-dimension a) 2)))
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
            (array-curry a (- rank 1)))))
        (if (not p1) (get-output-string p))))))
