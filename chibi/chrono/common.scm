
(define (is-julian-leap-year? year)
  (zero? (modulo year 4)))

(define (is-leap-year? year)
  (and (zero? (modulo year 4))
       (not (memv (modulo year 400) '(100 200 300)))))

;; returns the inclusive upper bound for the 0-based month.
(define (month-day-upper-bound year month)
  (case month
    ((2) (if (is-leap-year? year) 29 28))
    ((4 6 9 11) 30)
    (else 31)))

;; ignores leap years
(define cumulative-days-to-start-of-month
  '#(0 31 59 90 120 151 181 212 243 273 304 334))

;; cumulative number of days in the year up to but not including the
;; first day of the given 0-based month
(define (days-to-start-of-month year month)
  (+ (vector-ref cumulative-days-to-start-of-month (- month 1))
     (if (and (> month 2) (is-leap-year? year))
         1
         0)))

(define (julian-days-to-start-of-month year month)
  (+ (vector-ref cumulative-days-to-start-of-month (- month 1))
     (if (and (> month 2) (is-julian-leap-year? year))
         1
         0)))

;; given the 0-based day of the year [0, 365], returns the 0-based
;; month that day falls in
(define (month-of-day-of-year day-of-year year)
  (let ((day-of-year (if (and (> day-of-year (+ 31 28))
                              (is-leap-year? year))
                         (- day-of-year 1)
                         day-of-year)))
    ;; invariant: start[lo] <= day < start[hi+1]
    (+ 1
       (let lp ((lo 0)
                (hi 11))
         (if (>= lo hi)
             lo
             (let ((mid (quotient (+ lo hi) 2)))
               (cond
                ((< day-of-year
                    (vector-ref cumulative-days-to-start-of-month mid))
                 ;; start[lo] <= day < start[mid]
                 (lp lo (- mid 1)))
                ((= lo mid)
                 (if (< day-of-year
                        (vector-ref cumulative-days-to-start-of-month hi))
                     lo
                     hi))
                (else
                 ;; start[mid] <= day < start[hi+1]
                 (lp mid hi)))))))))

(define (gregorian->julian-day-number year month day)
  (let ((month-14/12 (quotient (- month 14) 12)))
    (+ (quotient (* 1461 (+ year 4800 month-14/12)) 4)
       (quotient (* 367 (- month 2 (* 12 month-14/12))) 12)
       (quotient (* -3 (quotient (+ year 4900 month-14/12) 100)) 4)
       day
       -32075)))

(define (julian->julian-day-number year month day)
  (+ (* 367 year)
     (quotient (* -7 (+ year 5001 (quotient (- month 9) 7))) 4)
     (quotient (* 275 month) 9)
     day
     1729777))

;; 0 is sunday, input gregorian
(define (day-of-week year month day)
  (modulo (+ 1 (gregorian->julian-day-number year month day)) 7))

(define (julian-day-of-week year month day)
  (modulo (+ 1 (julian->julian-day-number year month day)) 7))

;; The number of leap years occuring between 1970 and year (exclusive),
;; returning a negative value for years before 1970.
(define (leap-years-from-epoch-to year)
  (if (< year 1970)
      (+ -1
         (quotient (- year 1971) 4)
         (- (quotient (- year 1971) 100))
         (quotient (- year 1971) 400))
      (+ (quotient (- year 1969) 4)
         (- (quotient (- year 1969) 100))
         (quotient (- year 1969) 400))))

(define (julian-leap-years-from-epoch-to year)
  (quotient (- year 1969) 4))

;; Instants are the number of seconds (inexact iff nanosecond
;; precision is provided) since the Unix epoch (midnight 1st Jan 1970).
(define (gregorian->instant date)
  (+ (* 365 24 60 60 (- (datetime-year date) 1970))
     (* 24 60 60 (leap-years-from-epoch-to (datetime-year date)))
     (* 24 60 60 (days-to-start-of-month (datetime-year date)
                                         (datetime-month date)))
     (* 24 60 60 (- (datetime-day date)
                    (if (>= (datetime-year date) 1970) 1 0)))
     (* 60 60 (datetime-hour date))
     (* 60 (datetime-minute date))
     (datetime-second date)
     (if (zero? (datetime-nanosecond date))
         0
         (/ (datetime-nanosecond date) 1e9))))

(define (gregorian-date->instant date)
  (+ (* 365 24 60 60 (- (date-year date) 1970))
     (* 24 60 60 (leap-years-from-epoch-to (date-year date)))
     (* 24 60 60 (days-to-start-of-month (date-year date)
                                         (date-month date)))
     (* 24 60 60 (- (date-day date)
                    (if (>= (date-year date) 1970) 1 0)))))

(define year-of-the-council-of-nicea 325)

(define julian-only-leap-years-before-epoch
  (- (leap-years-from-epoch-to year-of-the-council-of-nicea)
     (julian-leap-years-from-epoch-to year-of-the-council-of-nicea)
     -1))

(define (julian-date->instant date)
  (+ (* 365 24 60 60 (- (julian-date-year date) 1970))
     (* 24 60 60 (quotient (- (julian-date-year date) 1969) 4))
     (* 24 60 60 julian-only-leap-years-before-epoch)
     (* 24 60 60 (julian-days-to-start-of-month (julian-date-year date)
                                                (julian-date-month date)))
     (* 24 60 60 (julian-date-day date))))

(define (instant->julian-date instant)
  (let* ((instant (- instant (* 24 60 60 julian-only-leap-years-before-epoch)))
         (exact-instant (exact (floor instant)))
         (nanosecond (- instant exact-instant))
         (year (+ 1970
                  (exact (floor (/ exact-instant (* 365.25 24 60 60))))))
         (secs (- exact-instant
                  (+ (* (- year 1970) 365 24 60 60)
                     (*  (quotient (- year 1969) 4) 24 60 60))))
         (day-of-year (quotient secs (* 24 60 60)))
         (month (month-of-day-of-year day-of-year year))
         (day (- day-of-year (julian-days-to-start-of-month year month) -1)))
    (make-julian-date year month day)))

(define (instant->gregorian instant . o)
  (let* ((exact-instant (exact (floor instant)))
         (nanosecond (- instant exact-instant))
         (year (+ 1970
                  (exact (floor (/ exact-instant (* 365.25 24 60 60))))))
         (secs (- exact-instant
                  (+ (* (- year 1970) 365 24 60 60)
                     (*  (leap-years-from-epoch-to year) 24 60 60))))
         (day-of-year (quotient secs (* 24 60 60)))
         (month (month-of-day-of-year day-of-year year))
         (day (- day-of-year (days-to-start-of-month year month) -1))
         (secs2 (- secs (* day-of-year 24 60 60)))
         (hour (quotient secs2 (* 60 60)))
         (secs3 (- secs2 (* hour 60 60)))
         (minute (quotient secs3 60))
         (second (- secs3 (* minute 60)))
         (res
          (make-datetime time-zone:utc year month day
                         hour minute second nanosecond 0)))
    (if (and (pair? o) (not (eq? (car o) time-zone:utc)))
        (temporal-in-time-zone res (car o))
        res)))

(define (instant->gregorian-date instant)
  (let* ((exact-instant (exact (floor instant)))
         (nanosecond (- instant exact-instant))
         (year (+ 1970
                  (exact (floor (/ exact-instant (* 365.25 24 60 60))))))
         (leap-years (leap-years-from-epoch-to year))
         (secs (- exact-instant
                  (+ (* (- year 1970) 365 24 60 60)
                     (* leap-years 24 60 60))))
         (day-of-year (quotient secs (* 24 60 60)))
         (month (month-of-day-of-year day-of-year year))
         (day (- day-of-year (days-to-start-of-month year month) -1)))
    (make-date year month day)))

;; Numbers are 1-based when traditionally presented as such directly
;; to the user (e.g. month or day of the month), but 0-based when
;; essentially an enum (e.g. day-of-week).  This differs from the
;; POSIX struct tm convention where months are 0-based.
(define-chronology chronology:gregorian
  (record DateTime)
  (constructor make-datetime)
  (predicate datetime?)
  (fields
   (time-zone datetime-zone (default time-zone:utc))
   (year datetime-year (lower -inf.0) (upper +inf.0))
   (month datetime-month (lower 1) (upper 12))
   (day datetime-day (lower 1) (upper 28)
        (get-upper (lambda (tz y m) (month-day-upper-bound y m))))
   (hour datetime-hour (lower 0) (upper 23))
   (minute datetime-minute (lower 0) (upper 59))
   (second datetime-second (lower 0) (upper 59))
   (nanosecond datetime-nanosecond (lower 0) (upper 999999999))
   (fold datetime-fold (lower 0) (upper 1)))
  (virtual
   (day-of-week
    (lambda (t)
      (day-of-week (datetime-year t) (datetime-month t) (datetime-day t))))
   (days-in-month
    (lambda (t)
      (month-day-upper-bound (datetime-year t) (datetime-month t))))
   (julian-day
    (lambda (t)
      (gregorian->julian-day-number
       (datetime-year t) (datetime-month t) (datetime-day t))))
   ;; (week-of-year )
   ;; (local-time-offset )
   )
  (to-instant
   gregorian->instant)
  (from-instant
   instant->gregorian)
  (format
   '(year "-" (fix0 2 month) "-" (fix0 2 day) "T"
          (fix0 2 hour) ":" (fix0 2 minute) ":" (fix0 2 second))))

(define-chronology chronology:gregorian-date
  (record Date)
  (constructor make-date)
  (predicate date?)
  (fields
   (year date-year (lower -inf.0) (upper +inf.0))
   (month date-month (lower 1) (upper 12))
   (day date-day (lower 1) (upper 28) (get-upper month-day-upper-bound)))
  (virtual
   (day-of-week
    (lambda (t)
      (day-of-week (date-year t) (date-month t) (date-day t))))
   (days-in-month
    (lambda (t)
      (month-day-upper-bound (date-year t) (date-month t))))
   (julian-day
    (lambda (t)
      (gregorian->julian-day-number
       (date-year t) (date-month t) (date-day t)))))
  (to-instant
   gregorian-date->instant)
  (from-instant
   instant->gregorian-date)
  (format
   '(year "-" (fix0 2 month) "-" (fix0 2 day))))

(define-chronology chronology:time
  (record Time)
  (constructor make-time)
  (predicate time?)
  (fields
   (hour time-hour (lower 0) (upper 23))
   (minute time-minute (lower 0) (upper 59))
   (second time-second (lower 0) (upper 59))
   (nanosecond time-nanosecond (lower 0) (upper 999999999)))
  (format
   '((fix0 2 hour) ":" (fix0 2 minute) ":" (fix0 2 second))))

(define-chronology chronology:julian-date
  (record Julian-Date)
  (constructor make-julian-date)
  (predicate julian-date?)
  (fields
   (year julian-date-year (lower -inf.0) (upper +inf.0))
   (month julian-date-month (lower 1) (upper 12))
   (day julian-date-day (lower 1) (upper 28) (get-upper month-day-upper-bound)))
  (to-instant
   julian-date->instant)
  (from-instant
   instant->julian-date)
  (format
   '(year "-" (fix0 2 month) "-" (fix0 2 day))))
