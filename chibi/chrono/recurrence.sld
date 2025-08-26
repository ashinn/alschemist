
(define-library (chibi chrono recurrence)
  (import (scheme base)
          (scheme char)
          (scheme list)
          (scheme time)
          (scheme vector)
          (scheme write)
          (srfi 2)
          (srfi 130)
          (srfi 227)
          (chibi assert)
          (chibi chrono))
  (export make-recurrence recurrence?
          recurrence-freq recurrence-interval recurrence-filters
          recurrence-count recurrence-start recurrence-end
          recurrence-chronology recurrence-time-zone
          recurrence->string string->recurrence
          recurrence-bounded? recurrence->generator
          recurrence->list)
  (include "recurrence.scm"))
