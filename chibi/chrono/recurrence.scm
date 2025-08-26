
(define-record-type Recurrence
  (%make-recurrence freq interval filters count start end chronology time-zone
                    checks steps)
  recurrence?
  (freq recurrence-freq)
  (interval recurrence-interval)
  ;; An alist of (field values ...).  Filters the allowed values for
  ;; fields less than freq if there is no filter for a field, the
  ;; start value is used.  If the field is virtual it is used as a
  ;; predicate to filter potential
  (filters recurrence-filters)
  (count recurrence-count)
  (start recurrence-start)
  (end recurrence-end)
  (chronology recurrence-chronology)
  (time-zone recurrence-time-zone)
  (checks recurrence-checks)
  (steps recurrence-steps))

(define make-recurrence
  (opt-lambda (freq
               (interval 1)
               (filters '())
               (count #f)
               (start #f)
               (end #f)
               (chronology chronology:gregorian)
               (zone #f))
    (when chronology
      (assert (chronology-known-field? chronology freq))
      (assert (every (lambda (filter)
                       (chronology-known-field? chronology (car filter)))
                     filters)))
    (%make-recurrence freq interval filters count start end chronology zone)))

(define (read-space-wrapped-line in)
  (let lp ((res '()))
    (let ((line (read-line in)))
      (cond
       ((eof-object? line)
        (if (pair? res)
            (string-concatenate-reverse res)
            line))
       (else
        (let ((res (cons (string-trim line) res)))
          (if (and (char? (peek-char in))
                   (char-whitespace? (peek-char in)))
              (lp res)
              (string-concatenate-reverse res))))))))

(define (parse-params str)
  (map (lambda (x)
         (cons (string->symbol (car x))
               (if (pair? (cdr x)) (cadr x) "")))
       (remove null?
               (map (lambda (s) (string-split s "="))
                    (string-split str ";")))))

(define (read-ical-line . o)
  (let* ((in (if (pair? o) (car o) (current-input-port)))
         (str (read-space-wrapped-line in)))
    (cond
     ((eof-object? str)
      str)
     ((equal? str "")
      (read-ical-line in))
     (else
      (let* ((colon (string-index str #\:))
             (semi-colon (string-index str
                                       #\;
                                       (string-cursor-start str)
                                       colon))
             (name (string->symbol
                    (substring/cursors str
                                       (string-cursor-start str)
                                       semi-colon)))
             (params (if (string-cursor=? colon semi-colon)
                         '()
                         (parse-params
                          (substring/cursors str semi-colon colon))))
             (value (substring/cursors str
                                       (string-cursor-next str colon)
                                       (string-cursor-end str))))
        (list name params value))))))

(define (parse-ical-lines str)
  (let ((in (open-input-string str)))
    (let lp ((res '()))
      (let ((line (read-ical-line in)))
        (if (eof-object? line)
            (reverse res)
            (lp (cons line res)))))))

(define (ical-freq->field chronology freq)
  ;; TODO: Consider non-standard extensions for other chronologies.
  ;; Using chronology fields is tricky because freq could be 'week.
  ;; Duration support should make that cleaner.
  (if (equal? freq "DAILY")
      'day
      (let* ((freq (string-downcase freq))
             (str (if (string-suffix? "ly" freq)
                     (string-drop-right freq 2)
                     freq)))
        (string->symbol str))))

(define (ical-by->field chronology by)
  ;; TODO: Consider non-standard extensions for other chronologies.
  (case by
    ((BYWEEKNO) 'week-of-year)
    ((BYYEARDAY) 'day-of-year)
    ((BYMONTH) 'month)
    ((BYMONTHDAY) 'day)
    ((BYDAY) 'day-of-week)
    ((BYHOUR) 'hour)
    ((BYMINUTE) 'minute)
    ((BYSECOND) 'second)
    (else #f)))

(define day-of-week-abbrevs
  '#("SU" "MO" "TU" "WE" "TH" "FR" "SA"))

(define (char-minus-or-numeric? ch)
  (or (eqv? ch #\-) (char-numeric? ch)))

(define (ical-by-value chronology value)
  (cond
   ((string->number value))
   ((vector-index (lambda (x) (equal? x value)) day-of-week-abbrevs))
   ((and (not (equal? "" value))
         (char-minus-or-numeric? (string-ref value 0)))
    (let ((non-digit (string-skip value char-minus-or-numeric?))
          (start (string-cursor-start value))
          (end (string-cursor-end value)))
      `(nth
        ,(ical-by-value chronology (substring/cursors value non-digit end))
        ,(string->number (substring/cursors value start non-digit)))))
   (else #f)))

(define string->recurrence
  (opt-lambda (str (chronology chronology:gregorian)
                   (time-zone #f))
    (define (parse-temporal temporal-str)
      (string->temporal temporal-str chronology))
    (let* ((info (parse-ical-lines str))
           (rrule (assq 'RRULE info))
           (dtstart (assq 'DTSTART info)))
      (assert rrule "no RRULE found in string" str)
      (let* ((params (parse-params (third rrule)))
             (start (and dtstart (parse-temporal (third dtstart))))
             (start (if time-zone
                        (temporal-in-time-zone start time-zone)
                        start))
             (freq (cond ((assq 'FREQ params) =>
                          (lambda (cell) (ical-freq->field chronology (cdr cell))))
                         (else (error "no FREQ found in RRULE" str))))
             (interval (cond ((assq 'INTERVAL params) =>
                              (lambda (cell) (string->number (cdr cell))))
                             (else 1)))
             (count (cond ((assq 'COUNT params) =>
                           (lambda (cell) (string->number (cdr cell))))
                          (else #f)))
             (end (cond ((assq 'UNTIL params) =>
                         (lambda (cell) (parse-temporal (cdr cell))))
                        (else #f)))
             (filters
              (filter-map
               (lambda (cell)
                 (cond
                  ((ical-by->field chronology (car cell))
                   => (lambda (field)
                        (let ((vals
                               (map (lambda (x) (ical-by-value chronology x))
                                    (string-split (cdr cell) ","))))
                          (if (and (pair? vals)
                                   (every (lambda (x)
                                            (and (pair? x) (eq? 'nth (car x))))
                                          vals))
                              (begin
                                (assert (every (lambda (x)
                                                 (equal? (cadr x)
                                                         (cadr (car vals))))
                                               (cdr vals)))
                                `(nth
                                  (,field ,(cadr (car vals)))
                                  ,@(append-map cddr vals)))
                              (cons field vals)))))
                  (else #f)))
               params))
             (checks
              (filter
               (lambda (f)
                 (or (eq? 'nth (car f))
                     (chronology-virtual-field? chronology (car f))
                     (chronology-field>? chronology (car f) freq)))
               filters))
             (steps
              (append
               (filter
                (lambda (f)
                  (and (chronology-explicit-field? chronology (car f))
                       (chronology-field<? chronology (car f) freq)))
                filters)
               ;; Add implied steps for virtual field checks on a finer
               ;; granularity than freq and not already a step.
               (filter-map
                (lambda (f)
                  (and-let*
                      ((f (if (eq? 'nth (car f)) (cadr f) f))
                       (vf (chronology-virtual-field? chronology (car f)))
                       (granularity (virtual-field-granularity vf))
                       ((chronology-field<? chronology granularity freq))
                       ((not (assq granularity filters)))
                       (field (chronology-explicit-field? chronology
                                                          granularity)))
                    (cons (chrono-field-name field)
                          (lambda (t) (chrono-field-range field t)))))
                filters))))
        (%make-recurrence
         freq interval filters count start end chronology time-zone
         checks steps)))))

(define (recurrence->string recurrence)
  (error "recurrence->string is unimplemented"))

(define (recurrence-bounded? recurrence)
  (and (or (recurrence-count recurrence)
           (recurrence-end recurrence))))

(define (list->generator ls)
  (lambda ()
    (if (null? ls)
        (eof-object)
        (let ((res (car ls)))
          (set! ls (cdr ls))
          res))))

;; Given ((field-a a1 a2 ...) (field-b b1 b2 ...))
;; return a generator thunk which returns the
;; cartesian product of the fields:
;;   (field-a a1 field-b b1)
;;   (field-a a1 field-b b2)
;;   (field-a a2 field-b b1)
;;   ...
;; one at a time, iterating over the smallest fields first, and
;; returning #f after all are exhausted.
(define (step-generator fields-with-values)
  (lambda (t)
    (define (make-gen fields)
      (if (null? fields)
          ;; Base case, no fields left, return a single '() once then stop.
          (list->generator '(()))
          (let* ((head-field-list (car fields))
                 (field-name (car head-field-list))
                 (current-values (if (procedure? (cdr head-field-list))
                                     ((cdr head-field-list) t)
                                     (cdr head-field-list)))
                 (tail-fields (cdr fields))
                 (tail-generator (make-gen tail-fields)))
            (lambda ()
              (if (null? current-values)
                  (eof-object)
                  (let ((tail (tail-generator)))
                    (when (eof-object? tail)
                      (set! current-values (cdr current-values))
                      (set! tail-generator (make-gen tail-fields))
                      (set! tail (tail-generator)))
                    (cond
                     ((null? current-values)
                      (eof-object))
                     (else
                      (cons field-name (cons (car current-values) tail))))))))))
    (make-gen fields-with-values)))

(define (chrono-field-range field t)
  (let ((lb (chrono-field-lower-bound field t))
        (ub (chrono-field-upper-bound field t)))
    (iota (+ 1 (- ub lb)) lb)))

(define (make-check check)
  (if (eq? 'nth (car check))
      (let ((base-check (make-check (cadr check)))
            (count 0))
        (lambda (t)
          (cond
           ((base-check t)
            (set! count (+ count 1))
            ;; TODO: support negative indices (last Friday, etc.)
            (member count (cddr check)))
           (else #f))))
      (lambda (t)
        (member (temporal-ref t (car check)) (cdr check)))))

(define recurrence->generator
  (opt-lambda (recurrence (start #f) (end #f))
    (let* ((chrono (recurrence-chronology recurrence))
           (start (or start
                      (recurrence-start recurrence)
                      (instant->temporal (current-second) chrono)))
           (end (or end (recurrence-end recurrence)))
           (t start)
           (count 0)
           (checks (map make-check (recurrence-checks recurrence)))
           (step ((step-generator (recurrence-steps recurrence)) t)))
      (lambda ()
        (let lp ()
          (cond
           ((and (recurrence-count recurrence)
                 (>= count (recurrence-count recurrence)))
            (eof-object))
           ((and end (temporal>=? t end))
            (eof-object))
           (else
            (let ((updates (step)))
              (cond
               ((eof-object? updates)
                (set! t (temporal-adjust t
                                         (recurrence-freq recurrence)
                                         (recurrence-interval recurrence)))
                (set! step ((step-generator (recurrence-steps recurrence)) t))
                (set! checks (map make-check (recurrence-checks recurrence)))
                (lp))
               (else
                (when (pair? updates)
                  ;; TODO: guard this if invalid date?
                  (set! t (apply temporal-update t updates)))
                (cond
                 ((every (lambda (check) (check t)) checks)
                  (set! count (+ count 1))
                  t)
                 (else
                  (lp)))))))))))))

(define recurrence->list
  (opt*-lambda (recurrence
                (start (recurrence-start recurrence))
                (end (recurrence-end recurrence)))
    (assert (or end (recurrence-count recurrence))
            "can't convert unbounded recurrence to list" recurrence)
    (let ((gen (recurrence->generator recurrence start end)))
      (let lp ((res '()))
        (let ((t (gen)))
          (if (or (eof-object? t) (not t))
              (reverse res)
              (lp (cons t res))))))))
