
;; Why have a notion of chronologies at all, as opposed to having
;; separate date and time classes for each separate calendar?
;;   1. standardized conversions (via a tai time offset)
;;   2. access via field names, allowing generic code such as parser/formatters
;;   3. better abstraction and sharing of code
;;
;; Note conversions are based on TAI time because it is unambiguous
;; (unix time is non-monotonic and represents two seconds with the
;; same value) and simplifies interval artihmetic.  R7RS
;; (current-second) returns TAI time, which we scale to nanoseconds
;; for convenience.  Unfortunately some systems make it difficult to
;; derive TAI time from the system clock, however if we know the
;; system uses leap second smoothing (and which window), the times are
;; in fact not ambiguous and we can recover accurately.
(define-record-type Chronology
  (make-chronology name fields virtual constructor to-instant from-instant
                   format messages)
  chronology?
  (name chronology-name)
  (fields chronology-fields)
  (virtual chronology-virtual)
  (constructor chronology-constructor)
  (to-instant chronology-to-instant)
  (from-instant chronology-from-instant)
  (format chronology-format)
  (messages chronology-messages))

(define default-chronology (make-parameter #f))

(define-record-type Chrono-Field
  (%make-chrono-field name getter lb ub get-lb get-ub updater adjuster default)
  chrono-field?
  (name chrono-field-name)
  (getter chrono-field-getter)
  (lb chrono-field-lb)
  (ub chrono-field-ub)
  (get-lb chrono-field-get-lb)
  (get-ub chrono-field-get-ub)
  (updater chrono-field-updater)
  (adjuster chrono-field-adjuster)
  (default chrono-field-default))

(define (make-chrono-field name getter . o)
  (let-optionals* o ((lb #f) (ub #f) (get-lb #f) (get-ub #f)
                     (updater #f) (adjuster #f) (default #f))
    (%make-chrono-field name getter lb ub get-lb get-ub updater adjuster default)))

(define (chrono-field-lower-bound field reverse-prev-values)
  (cond
   ((chrono-field-get-lb field)
    => (lambda (get-lb) (apply get-lb (reverse reverse-prev-values))))
   ((chrono-field-lb field))
   (else #f)))

(define (chrono-field-upper-bound field reverse-prev-values)
  (cond
   ((chrono-field-get-ub field)
    => (lambda (get-ub) (apply get-ub (reverse reverse-prev-values))))
   ((chrono-field-ub field))
   (else #f)))

;; validates fields are in [lb, ub).
;; lb and ub are absolute bounds - anything within them is always ok.
;; get-lb/ub are dynamic bounds, used only if lb and ub are missing or fail.
(define (chrono-field-valid? field value reverse-prev-values)
  (and (cond ((chrono-field-lb field)
              => (lambda (lb)
                   (or (<= lb value)
                       (cond
                        ((chrono-field-get-lb field)
                         => (lambda (get-lb)
                              (<= (apply get-lb (reverse reverse-prev-values))
                                  value)))
                        (else #f)))))
             ((chrono-field-get-lb field)
              => (lambda (get-lb)
                   (<= (apply get-lb (reverse reverse-prev-values))
                       value)))
             (else #t))
       (cond ((chrono-field-ub field)
              => (lambda (ub)
                   (or (<= value ub)
                       (cond
                        ((chrono-field-get-ub field)
                         => (lambda (get-ub)
                              (<= value
                                  (apply get-ub
                                         (reverse reverse-prev-values)))))
                        (else #f)))))
             ((chrono-field-get-ub field)
              => (lambda (get-ub)
                   (<= value (apply get-ub (reverse reverse-prev-values)))))
             (else #t))))

(define-syntax chrono-field
  (syntax-rules (lower upper get-lower get-upper updater adjuster default)
    ((_ name get lb ub get-lb get-ub up adj dflt ())
     (%make-chrono-field name get lb ub get-lb get-ub up adj dflt))
    ((_ name get lb ub get-lb get-ub up adj dflt ((lower x) . rest))
     (chrono-field name get x ub get-lb get-ub up adj dflt rest))
    ((_ name get lb ub get-lb get-ub up adj dflt ((upper x) . rest))
     (chrono-field name get lb x get-lb get-ub up adj dflt rest))
    ((_ name get lb ub get-lb get-ub up adj dflt ((get-lower x) . rest))
     (chrono-field name get lb ub x get-ub up adj dflt rest))
    ((_ name get lb ub get-lb get-ub up adj dflt ((get-upper x) . rest))
     (chrono-field name get lb ub get-lb x up adj dflt rest))
    ((_ name get lb ub get-lb get-ub up adj dflt ((updater x) . rest))
     (chrono-field name get lb ub get-lb get-ub up adj dflt rest))
    ((_ name get lb ub get-lb get-ub up adj dflt ((adjuster x) . rest))
     (chrono-field name get lb ub get-lb get-ub up adj dflt rest))
    ((_ name get lb ub get-lb get-ub up adj dflt ((default x) . rest))
     (chrono-field name get lb ub get-lb get-ub up adj x rest))
    ((_ name get lb ub get-lb get-ub up adj dflt ((other . x) . rest))
     (syntax-error "unknown chrono-field attribute" (other . x)))
    ))

(define-syntax define-chrono
  (syntax-rules (record constructor predicate fields virtual
                        to-instant from-instant format messages)
    ((define-chrono name
       instance-rtd instance-constructor instance-predicate
       ((field getter spec ...) ...)
       ((vfield vgetter) ...)
       to
       from
       fmt
       msg
       ())
     (begin
       (define-record-type instance-rtd
         (instance-constructor field ...)
         instance-predicate
         (field getter) ...)
       (define name
         (let ((res
                (make-chronology
                 'name
                 (list
                  (chrono-field 'field getter #f #f #f #f #f #f #f (spec ...))
                  ...)
                 (list (cons 'vfield vgetter) ...)
                 instance-constructor
                 to
                 from
                 fmt
                 msg)))
           (hash-table-set! chronologies instance-rtd res)
           res))))
    ((define-chrono name r c p f v to from fmt msg ((record rtd) . rest))
     (define-chrono name rtd c p f v to from fmt msg rest))
    ((define-chrono name r c p f v to from fmt msg ((constructor cons) . rest))
     (define-chrono name r cons p f v to from fmt msg rest))
    ((define-chrono name r c p f v to from fmt msg ((predicate pred) . rest))
     (define-chrono name r c pred f v to from fmt msg rest))
    ((define-chrono name r c p f v to from fmt msg ((fields . ls) . rest))
     (define-chrono name r c p ls v to from fmt msg rest))
    ((define-chrono name r c p f v to from fmt msg ((virtual . ls) . rest))
     (define-chrono name r c p f ls to from fmt msg rest))
    ((define-chrono name r c p f v to from fmt msg ((to-instant to-i) . rest))
     (define-chrono name r c p f v to-i from fmt msg rest))
    ((define-chrono name r c p f v to from fmt msg ((from-instant fi) . rest))
     (define-chrono name r c p f v to fi fmt msg rest))
    ((define-chrono name r c p f v to from fmt msg ((format fo) . rest))
     (define-chrono name r c p f v to from fo msg rest))
    ((define-chrono name r c p f v to from fmt msg ((messages . ls) . rest))
     (define-chrono name r c p f v to from fmt `ls rest))
    ))

(define-syntax define-chronology
  (syntax-rules ()
    ((define-chronology name . rest)
     (define-chrono name #f #f #f () () #f #f #f '() rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (current-nanosecond)
  (exact (floor (* 1e9 (current-second)))))

(define (temporal? x)
  (and (temporal-chronology x) #t))

(define (temporal-ref t field)
  (let lp ((fields (chronology-fields (temporal-chronology t))))
    (cond
     ((null? fields)
      (let lp ((fields (chronology-virtual (temporal-chronology t))))
        (cond
         ((null? fields) (error "unknown field" field t))
         ((eq? field (caar fields)) ((cdar fields) t))
         (else (lp (cdr fields))))))
     ((eq? field (chrono-field-name (car fields)))
      ((chrono-field-getter (car fields)) t))
     (else
      (lp (cdr fields))))))

;; returns true iff all of the values in ls are valid for their
;; corresponding fields, given the prev fields (in reverse order).
(define (temporal-valid-fields? ls fields prev)
  (cond
   ((null? fields)
    #t)
   ((not (chrono-field-valid? (car fields) (car ls) prev))
    #f)
   (else
    (temporal-valid-fields? (cdr ls) (cdr fields) (cons (car ls) prev)))))

;; op must be <= or >= or =
(define (temporal-cmp op a b)
  (if (eq? (temporal-chronology a) (temporal-chronology b))
      (let lp ((ls-a (temporal-fields a))
               (ls-b (temporal-fields b)))
        (cond
         ((null? ls-a) (null? ls-b))
         ((null? ls-b) #f)
         ((not (number? (car ls-a)))
          (if (eq? (car ls-a) (car ls-b))
              (lp (cdr ls-a) (cdr ls-b))
              (op (temporal->instant a) (temporal->instant b))))
         ((op (car ls-a) (car ls-b))
          (lp (cdr ls-a) (cdr ls-b)))
         (else
          #f)))
      (op (temporal->instant a) (temporal->instant b)))
  (op (temporal->instant a) (temporal->instant b)))

(define (temporal=? a b)
  (temporal-cmp = a b))
(define (temporal<? a b)
  (temporal-cmp >= b a))
(define (temporal>? a b)
  (temporal-cmp <= b a))
(define (temporal<=? a b)
  (temporal-cmp <= a b))
(define (temporal>=? a b)
  (temporal-cmp >= a b))

;; signals an error on out of range
(define (temporal-update t field value . o)
  (let ((chronology (temporal-chronology t)))
    ;; TODO: apply all updates before validation
    ((lambda (res) (if (pair? o) (apply temporal-update res o) res))
     (let lp ((ls (temporal->list t))
              (fields (chronology-fields chronology))
              (prev '()))
       (cond
        ((null? fields)
         (error "unknown field" field))
        ((eq? field (chrono-field-name (car fields)))
         (cond
          ((chrono-field-updater (car fields))
           => (lambda (updater) (updater t value)))
          ((chrono-field-valid? (car fields) value prev)
           ;; validate remaining fields
           (if (temporal-valid-fields? (cdr ls) (cdr fields) (cons value prev))
               (apply (chronology-constructor chronology)
                      `(,@(reverse prev) ,value ,@(cdr ls)))
               (error "field value invalidates other field"
                      t ls field value prev (chrono-field-name (car fields)))))
          (else
           (error "field value out of range" t field value))))
        (else
         (lp (cdr ls) (cdr fields) (cons (car ls) prev))))))))

;; Takes a list of valid prev fields with values of temporal t, and
;; prepends the new value onto the list.  If value is over/under the
;; bounds for field, carry/borrow from the prev-values as needed.
(define (carry-adjust prev-fields prev-values field value)
  (let ((lb (chrono-field-lower-bound field prev-values))
        (ub (chrono-field-upper-bound field prev-values)))
    (cond
     ((< value lb)  ; borrow
      (let* ((prev-values
              (carry-adjust (cdr prev-fields) (cdr prev-values)
                            (car prev-fields) (- (car prev-values) 1)))
             (ub (chrono-field-upper-bound field prev-values)))
        (carry-adjust prev-fields prev-values field (- ub value -1))))
     ((> value ub) ; carry
      (let* ((prev-values
              (carry-adjust (cdr prev-fields) (cdr prev-values)
                            (car prev-fields) (+ (car prev-values) 1)))
             (new-ub (chrono-field-upper-bound field prev-values))
             (new-lb (chrono-field-lower-bound field prev-values))
             (new-value (- value (- ub lb -1))))
        (carry-adjust prev-fields prev-values field new-value)))
     (else
      (cons value prev-values)))))

;; borrows/carries from larger fields as needed, caps field accordingly
(define (temporal-adjust t field increment)
  (let ((chronology (temporal-chronology t)))
    (let lp ((ls (temporal->list t))
             (fields (chronology-fields chronology))
             (prev '())
             (prev-fields '()))
      (cond
       ((null? fields)
        (error "unknown field" field))
       ((eq? field (chrono-field-name (car fields)))
        (cond
         ((chrono-field-adjuster (car fields))
          => (lambda (adjuster) (adjuster t increment)))
         (else
          (let* ((value (+ (car ls) increment))
                 (prev2 (carry-adjust prev-fields prev (car fields) value)))
            ;; TODO: cap trailing fields
            (apply (chronology-constructor chronology)
                   `(,@(reverse prev2) ,@(cdr ls)))))))
       (else
        (lp (cdr ls)
            (cdr fields)
            (cons (car ls) prev)
            (cons (car fields) prev-fields)))))))

(define chronologies (make-hash-table eq?))

(define (temporal-chronology t)
  (hash-table-ref chronologies (record-rtd t)))

(define (temporal-fields t)
  (chronology-fields (temporal-chronology t)))

(define (temporal->instant t)
  (let ((chronology (hash-table-ref chronologies (record-rtd t))))
    ((chronology-to-instant chronology) t)))

(define (instant->temporal instant . o)
  (let ((chronology (if (pair? o) (car o) (default-chronology))))
    (assert (chronology? chronology))
    ((chronology-from-instant chronology) instant)))

(define (temporal-in-chronology t chronology)
  (instant->temporal (temporal->instant t) chronology))

(define (temporal->list t)
  (map (lambda (field) ((chrono-field-getter field) t))
       (temporal-fields t)))

(define (temporal->alist t)
  (map (lambda (field)
         (cons (chrono-field-name field)
               ((chrono-field-getter field) t)))
       (temporal-fields t)))

(define (list->temporal ls . o)
  (let ((chronology (if (pair? o) (car o) (default-chronology))))
    (assert (chronology? chronology))
    (apply (chronology-constructor chronology) ls)))

;; Like assq but returns a pair of the (reversed) left part of the
;; alist preceding the matched cell, and the matched cell.
(define (assq-split key orig-ls)
  (let lp ((ls1 orig-ls))
    (cond ((null? ls1) #f)
          ((eq? key (caar ls1))
           (let lp ((ls2 orig-ls) (left '()))
             (cond ((eq? ls2 ls1) (cons left ls1))
                   ((null? ls2) (error "can't happen"))
                   (else (lp (cdr ls2) (cons (car ls2) left))))))
          (else (lp (cdr ls1))))))

(define (try-alist->temporal ls pass fail . o)
  (let ((chronology (if (pair? o) (car o) (default-chronology))))
    (define (finish rev-args ls)
      (let ((res (list->temporal (reverse rev-args) chronology)))
        (let lp ((ls ls))
          (cond
           ((null? ls) (pass res))
           ((assq (caar ls) (chronology-virtual chronology))
            => (lambda (cell)
                 (let ((expected ((cdr cell) res)))
                   (if (equal? (cdar ls) expected)
                       (lp (cdr ls))
                       (fail res
                             (list "invalid virtual field, expected "
                                   expected " but got " (cdar ls)))))))
           (else
            (fail res (list "unknown field in chronology" chronology (car ls)))
            )))))
    (assert (chronology? chronology))
    (let lp ((fields (chronology-fields chronology))
             (args '())
             (ls ls))
      (cond
       ((null? fields)
        (finish args ls))
       ((assq-split (chrono-field-name (car fields)) ls)
        => (lambda (left+right)
             (let ((left (car left+right))
                   (right (cdr left+right)))
               (lp (cdr fields)
                   (cons (cdar right) args)
                   (append left (cdr right))))))
       ((chrono-field-get-lb (car fields))
        => (lambda (get-lb)
             (lp (cdr fields)
                 (cons (apply get-lb (reverse args)) args)
                 ls)))
       ((chrono-field-lb (car fields))
        => (lambda (lb)
             (lp (cdr fields) (cons lb args) ls)))
       ((chrono-field-default (car fields))
        => (lambda (default)
             (lp (cdr fields) (cons default args) ls)))
       (else  ;; hope the constructor has the proper defaults
        (finish args ls))))))

(define (alist->temporal ls . o)
  (let-optionals o ((chronology (default-chronology))
                    (strict? #f))
    (try-alist->temporal
     ls
     values
     (lambda (res err) (if (and res (not strict?)) res (apply error err)))
     chronology)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
