
;;> The R7RS small language provides a single notion of time in
;;> \scheme{current-second}, the number of seconds (possibly negative,
;;> possibly inexact) since the POSIX epoch.  This is simple and
;;> efficient, and since it uses TAI time (i.e. counts leap seconds),
;;> is unambiguous and able to express any time point.  However, it is
;;> difficult for humans to work with directly, since we are
;;> accustomed to dividing time into convenient units with calendars
;;> and clocks.
;;>
;;> This library builds on these seconds, which we call "instants," by
;;> adding two types: "chronologies," representing a structuring of
;;> time with a given calendar and/or clock, and "temporals,"
;;> representing an instant within a chronology.  We further provide
;;> generic utilities for comparisons and modifications to these
;;> temporals within any chronology.
;;>
;;> Note the libraries are factored for clean and minimal
;;> dependencies, and this base library has no notion of a default
;;> chronology.  The standard Gregorian chronology is provided by
;;> \scheme{(chibi chrono common)} and re-exported by
;;> \scheme{(chibi chrono)}, but other chronologies are available.
;;>
;;> You may ask, why have a notion of chronologies at all, as opposed
;;> to having separate date and time classes for each separate
;;> calendar?  The generic API allows a number of conveniences, such as:
;;>
;;> \itemlist[
;;>  \item[standardized conversions (via a tai time offset)]
;;>  \item[access via field names, allowing generic code such as parser/formatters]
;;>  \item[better abstraction and sharing of code]]

(define-record-type Chronology
  (make-chronology name fields virtual durations constructor
                   to-instant from-instant format messages)
  chronology?
  (name chronology-name)
  (fields chronology-fields)
  (virtual chronology-virtual)
  (durations chronology-durations)
  (constructor chronology-constructor)
  (to-instant chronology-to-instant)
  (from-instant chronology-from-instant)
  (format chronology-format)
  (messages chronology-messages))

(define-record-type Chrono-Field
  (%make-chrono-field name getter lb ub get-lb get-ub updater adjuster default parser formatter)
  chrono-field?
  (name chrono-field-name)
  (getter chrono-field-getter)
  (lb chrono-field-lb)
  (ub chrono-field-ub)
  (get-lb chrono-field-get-lb)
  (get-ub chrono-field-get-ub)
  (updater chrono-field-updater)
  (adjuster chrono-field-adjuster)
  (default chrono-field-default)
  (parser chrono-field-parser)
  (formatter chrono-field-formatter))

(define (make-chrono-field name getter . o)
  (let-optionals* o ((lb #f) (ub #f) (get-lb #f) (get-ub #f)
                     (updater #f) (adjuster #f) (default #f)
                     (parser #f) (formatter #f))
    (%make-chrono-field name getter lb ub get-lb get-ub updater adjuster default parser formatter)))

(define (chronology-field-index chronology field)
  (let lp ((ls (chronology-fields chronology))
           (i 0))
    (cond
     ((null? ls) #f)
     ((eq? field (if (symbol? field) (chrono-field-name (car ls)) (car ls))) i)
     (else (lp (cdr ls) (+ i 1))))))

(define (chrono-field-lower-bound field reverse-prev-values)
  (cond
   ((and (chrono-field-get-lb field)
         (temporal? reverse-prev-values))
    (let* ((chrono (temporal-chronology reverse-prev-values))
           (vals (take (temporal->list reverse-prev-values)
                       (chronology-field-index chrono field))))
      (chrono-field-lower-bound field (reverse vals))))
   ((chrono-field-get-lb field)
    => (lambda (get-lb) (apply get-lb (reverse reverse-prev-values))))
   ((chrono-field-lb field))
   (else #f)))

(define (chrono-field-upper-bound field reverse-prev-values)
  (cond
   ((and (chrono-field-get-ub field)
         (temporal? reverse-prev-values))
    (let* ((chrono (temporal-chronology reverse-prev-values))
           (vals (take (temporal->list reverse-prev-values)
                       (chronology-field-index chrono field))))
      (chrono-field-upper-bound field (reverse vals))))
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
  (syntax-rules (lower upper get-lower get-upper updater adjuster default parser formatter)
    ((_ name get lb ub get-lb get-ub up adj dflt p f ())
     (%make-chrono-field name get lb ub get-lb get-ub up adj dflt p f))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((lower x) . rest))
     (chrono-field name get x ub get-lb get-ub up adj dflt p f rest))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((upper x) . rest))
     (chrono-field name get lb x get-lb get-ub up adj dflt p f rest))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((get-lower x) . rest))
     (chrono-field name get lb ub x get-ub up adj dflt p f rest))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((get-upper x) . rest))
     (chrono-field name get lb ub get-lb x up adj dflt p f rest))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((updater x) . rest))
     (chrono-field name get lb ub get-lb get-ub up adj dflt p f rest))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((adjuster x) . rest))
     (chrono-field name get lb ub get-lb get-ub up adj dflt p f rest))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((default x) . rest))
     (chrono-field name get lb ub get-lb get-ub up adj x p f rest))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((parser x) . rest))
     (chrono-field name get lb ub get-lb get-ub up adj dflt x f rest))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((formatter x) . rest))
     (chrono-field name get lb ub get-lb get-ub up adj dflt p x rest))
    ((_ name get lb ub get-lb get-ub up adj dflt p f ((other . x) . rest))
     (syntax-error "unknown chrono-field attribute" (other . x)))
    ))

(define-record-type Virtual-Field
  (%make-virtual-field name getter granularity)
  virtual-field?
  (name virtual-field-name)
  (getter virtual-field-getter)
  (granularity virtual-field-granularity))

(define make-virtual-field
  (opt-lambda (name getter (granularity #f))
    (%make-virtual-field name getter granularity)))

(define-syntax define-chrono
  (syntax-rules (record constructor predicate fields virtual durations
                        to-instant from-instant format messages)
    ((define-chrono name
       instance-rtd instance-constructor instance-predicate
       ((field getter spec ...) ...)
       ((vfield . vspec) ...)
       (duration ...)
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
                  (chrono-field 'field getter
                                #f #f #f #f #f #f #f #f #f (spec ...))
                  ...)
                 (list (make-virtual-field 'vfield . vspec) ...)
                 '(duration ...)
                 instance-constructor
                 to
                 from
                 fmt
                 msg)))
           (hash-table-set! chronologies instance-rtd res)
           res))))
    ((define-chrono name r c p f v d to from fmt msg ((record rtd) . rest))
     (define-chrono name rtd c p f v d to from fmt msg rest))
    ((define-chrono name r c p f v d to from fmt msg ((constructor cons) . rest))
     (define-chrono name r cons p f v d to from fmt msg rest))
    ((define-chrono name r c p f v d to from fmt msg ((predicate pred) . rest))
     (define-chrono name r c pred f v d to from fmt msg rest))
    ((define-chrono name r c p f v d to from fmt msg ((fields . ls) . rest))
     (define-chrono name r c p ls v d to from fmt msg rest))
    ((define-chrono name r c p f v d to from fmt msg ((virtual . ls) . rest))
     (define-chrono name r c p f ls d to from fmt msg rest))
    ((define-chrono name r c p f v d to from fmt msg ((to-instant to-i) . rest))
     (define-chrono name r c p f v d to-i from fmt msg rest))
    ((define-chrono name r c p f v d to from fmt msg ((from-instant fi) . rest))
     (define-chrono name r c p f v d to fi fmt msg rest))
    ((define-chrono name r c p f v d to from fmt msg ((format fo) . rest))
     (define-chrono name r c p f v d to from fo msg rest))
    ((define-chrono name r c p f v d to from fmt msg ((messages m) . rest))
     (define-chrono name r c p f v d to from fmt m rest))
    ((define-chrono name r c p f v d to from fmt msg ((durations . ls) . rest))
     (define-chrono name r c p f v ls to from fmt msg rest))
    ))

(define-syntax define-chronology
  (syntax-rules ()
    ((define-chronology name . rest)
     (define-chrono name #f #f #f () () () #f #f #f '() rest))))

(define (chronology-explicit-field? chronology field)
  (find (lambda (f) (eq? field (chrono-field-name f)))
        (chronology-fields chronology)))

(define (chronology-virtual-field? chronology field)
  (find (lambda (f) (eq? field (virtual-field-name f)))
        (chronology-virtual chronology)))

(define (chronology-known-field? chronology field)
  (or (chronology-explicit-field? chronology field)
      (chronology-virtual-field? chronology field)))

;; virtual fields are unordered
(define (chronology-field-cmp chronology field1 field2)
  (cond
   ((eq? field1 field2) 0)
   ((assq field1 (chronology-durations chronology))
    => (lambda (dur)
         (let ((res (chronology-field-cmp chronology (second dur) field2)))
           (if (zero? res) 1 res))))
   ((assq field2 (chronology-durations chronology))
    => (lambda (dur)
         (let ((res (chronology-field-cmp chronology field1 (second dur))))
           (if (zero? res) -1 res))))
   (else
    (let lp ((ls (chronology-fields chronology)))
      (cond
       ((null? ls) 0)
       ((eq? field1
             (if (chrono-field? field1) (car ls) (chrono-field-name (car ls))))
        1)
       ((eq? field2
             (if (chrono-field? field2) (car ls) (chrono-field-name (car ls))))
        -1)
       (else
        (lp (cdr ls))))))))

(define (chronology-field<? chronology field1 field2)
  (negative? (chronology-field-cmp chronology field1 field2)))
(define (chronology-field>? chronology field1 field2)
  (positive? (chronology-field-cmp chronology field1 field2)))

(define-record-type Duration
  (%make-duration field-counts chronology)
  duration?
  (field-counts duration-field-counts)
  (chronology duration-chronology))

(define (duration->alist duration)
  (map (lambda (cell) (cons (car cell) (cdr cell)))
       (duration-field-counts duration)))

(define make-duration
  (opt-lambda (field-counts (chronology #f))
    (when chronology
      (for-each
       (lambda (field-count)
         (assert
          (or (chronology-known-field? chronology (car field-count))
              (assq (car field-count) (chronology-durations chronology)))))
       field-counts))
    (%make-duration field-counts chronology)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;> Returns the exact current number of nanoseconds since the POSIX epoch.
(define (current-nanosecond)
  (exact (floor (* 1e9 (current-second)))))

;;> Returns \scheme{#t} iff \var{x} is a temporal.
(define (temporal? x)
  (and (record? x) (hash-table-contains? chronologies (record-rtd x)) #t))

;;> Returns the value of \var{field} in the temporal \var{t}, or
;;> signals an error if \var{field} is unknown in \var{t}'s
;;> chronology.
(define (temporal-ref t field)
  (let lp ((fields (chronology-fields (temporal-chronology t))))
    (cond
     ((null? fields)
      (let lp ((fields (chronology-virtual (temporal-chronology t))))
        (cond
         ((null? fields) (error "unknown field" field t))
         ((eq? field (virtual-field-name (car fields)))
          ((virtual-field-getter (car fields)) t))
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

(define (temporal-min a . o)
  (let lp ((ls o) (res a))
    (if (null? ls)
        res
        (lp (cdr ls)
            (if (temporal<? (car ls) res) (car ls) res)))))

(define (temporal-max a . o)
  (let lp ((ls o) (res a))
    (if (null? ls)
        res
        (lp (cdr ls)
            (if (temporal>? (car ls) res) (car ls) res)))))

;;> Absolute ordering procedures for temporals, which need not use the
;;> same chronology.
;;/

;;> Returns a new temporal which is the same as \var{t} but setting
;;> the given \var{field}s to thei \var{value}s.  Signals an error if
;;> the result is not valid within \var{t}'s chronology, such as
;;> having too many days in the resulting month.
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
      (if (null? prev-fields)
          (error "can't borrow for largest field" field value)
          (let* ((prev-values
                  (carry-adjust (cdr prev-fields) (cdr prev-values)
                                (car prev-fields) (- (car prev-values) 1)))
                 (ub (chrono-field-upper-bound field prev-values)))
            (carry-adjust prev-fields prev-values field (+ ub value 1)))))
     ((> value ub) ; carry
      (if (null? prev-fields)
          (error "can't carry for largest field" field value)
          (let* ((prev-values
                  (carry-adjust (cdr prev-fields) (cdr prev-values)
                                (car prev-fields) (+ (car prev-values) 1)))
                 (new-ub (chrono-field-upper-bound field prev-values))
                 (new-lb (chrono-field-lower-bound field prev-values))
                 (new-value (- value (- ub lb -1))))
            (carry-adjust prev-fields prev-values field new-value))))
     (else
      (cons value prev-values)))))

;;> Returns a new temporal which is the same as \var{t}, but
;;> incrementing \var{field} by the given amount, borrowing or
;;> carrying from larger fields as needed and capping lesser fields to
;;> their maximum value.  Correspondingly, unlike
;;> \scheme{temporal-update}, won't signal an out of range error.
(define (temporal-adjust t field increment)
  (let ((chronology (temporal-chronology t)))
    (cond
     ((assq field (chronology-durations chronology))
      => (lambda (dur)
           (temporal-adjust t (second dur) (* increment (third dur)))))
     (else
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
              (cons (car fields) prev-fields)))))))))

(define (temporal-add-duration t duration)
  (assert (or (not (duration-chronology duration))
              (eq? (temporal-chronology t) (duration-chronology duration))))
  (fold (lambda (cell t) (temporal-adjust t (car cell) (cdr cell)))
        t
        (duration-field-counts duration)))

(define chronologies (make-hash-table eq?))

;;> Returns the chronology associated with temporal \var{t}.
(define (temporal-chronology t)
  (assert (temporal? t))
  (hash-table-ref chronologies (record-rtd t)))

;;> Returns the list of chronology fields for temporal \var{t}.
(define (temporal-fields t)
  (chronology-fields (temporal-chronology t)))

;;> Converts temporal \var{t} to an instant.
(define (temporal->instant t)
  (let ((chronology (temporal-chronology t)))
    ((chronology-to-instant chronology) t)))

;;> Converts \var{instant} to a temporal in \var{chronology}.
(define (chronology-instant->temporal instant chronology)
  (assert (chronology? chronology))
  ((chronology-from-instant chronology) instant))

;;> Converts temporal \var{t} to a temporal in a (possibly different)
;;> \var{chronology}.
(define (temporal-in-chronology t chronology)
  (chronology-instant->temporal (temporal->instant t) chronology))

;;> Returns an ordered list of the field values of temporal \var{t}.
;;> By convention, fields are in "big-endian" order (i.e. largest
;;> units such as years come first).
(define (temporal->list t)
  (map (lambda (field) ((chrono-field-getter field) t))
       (temporal-fields t)))

;;> Returns an alist of the field names and values of temporal
;;> \var{t}, in the same order as \scheme{temporal->list}.
(define (temporal->alist t)
  (map (lambda (field)
         (cons (chrono-field-name field)
               ((chrono-field-getter field) t)))
       (temporal-fields t)))

;;> Returns a temporal in \var{chronology} with field values in
;;> \var{ls} in the same order as \scheme{temporal->list}.
(define (chronology-list->temporal ls chronology)
  (assert (chronology? chronology))
  (apply (chronology-constructor chronology) ls))

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

(define (assq-partition key ls)
  (partition (lambda (x) (eq? key (car x))) ls))

(define (solve-equations ls chrono-field)
  (if (null? ls)
      0
      (let lp ((x 0))
        (let ((y ((caar ls) x)))
          (cond
           ((every (lambda (eqn) ((cadr eqn) y)) (cdr ls))
            y)
           ((> x 10000)
            ;;(error "equations don't resolve")
            #f)
           (else
            (lp (+ x 1))))))))

;;> Attempts to convert the given alist of field names to values to a
;;> temporal in \var{chronology}.  Virtual fields in the alist are not
;;> normative, but used for validation, e.g. if day-of-week is 0 and
;;> the result from other fields results in a temporal not on a
;;> Sunday, we fail.  Calls \var{(pass result)} if successful, and
;;> \var{(fail result msg)} otherwise.
(define (chronology-try-alist->temporal ls pass fail chronology)
  (define (finish rev-args ls)
    (let ((res (chronology-list->temporal (reverse rev-args) chronology)))
      (let lp ((ls ls))
        (cond
         ((null? ls) (pass res))
         ((find (lambda (x) (eq? (caar ls) (virtual-field-name x)))
                (chronology-virtual chronology))
          => (lambda (vf)
               (let ((expected ((virtual-field-getter vf) res)))
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
             (if (and (pair? (cdar right))
                      (eq? 'solve (car (cdar right))))
                 ;; TODO: collect all eqns
                 (let-values (((eqns rest)
                               (assq-partition (chrono-field-name (car fields))
                                               (cdr right))))
                   (lp (cdr fields)
                       (cons (solve-equations (cons (cdr (cdar right))
                                                    (map cddr eqns))
                                              (car fields))
                             args)
                       (append left rest)))
                 (lp (cdr fields)
                     (cons (cdar right) args)
                     (append left (cdr right)))))))
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
     (else ;; hope the constructor has the proper defaults
      (finish args ls)))))

;;> Converts the given alist of field names to values to a temporal in
;;> \var{chronology}.  If optional \var{strict?} is true, signals an
;;> error if virtual fields fail to validate, otherwise ignores them.
(define (chronology-alist->temporal ls chronology . o)
  (let-optionals o ((strict? #f))
    (chronology-try-alist->temporal
     ls
     values
     (lambda (res err) (if (and res (not strict?)) res (apply error err)))
     chronology)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
