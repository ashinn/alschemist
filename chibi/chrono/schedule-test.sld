
(define-library (chibi chrono schedule-test)
  (import (scheme base)
          (chibi chrono)
          (chibi chrono recurrence)
          (chibi chrono schedule)
          (chibi test))
  (export run-tests)
  (begin
    (define (@ str) (string->temporal str))
    (define (->date-string t)
      (temporal->string t '(year "-" (fit0 2 month) "-" (fit0 2 day))))
    (define (run-tests)
      (test-group "(chibi chrono schedule)"
        (test '((m . "2050-01-01") (q . "2050-01-01") (m . "2050-02-01")
                (m . "2050-03-01") (m . "2050-04-01") (q . "2050-04-01")
                (m . "2050-05-01") (m . "2050-06-01") (m . "2050-07-01")
                (q . "2050-07-01"))
            (let-values
                (((res tt)
                  (time-table-project
                   (time-table
                    ;; Note these have overlapping dates, but are
                    ;; added in order, so the latter schedule
                    ;; (monthly) will be applied first.  After that,
                    ;; the monthly again runs two more times before
                    ;; the next overlapping date, so is still
                    ;; scheduled first.
                    (make-quarterly-schedule "quarterly" 'q (@ "2050-01-01"))
                    (recurrence->schedule
                     (make-recurrence 'month 1 '() #f (@ "2050-01-01"))
                     "monthly"
                     'm))
                   (@ "2050-08-01")
                   (lambda (when what acc)
                     (cons (cons what (->date-string when)) acc))
                   '())))
              (reverse res)))))))
