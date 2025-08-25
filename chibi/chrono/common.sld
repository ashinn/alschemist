
(define-library (chibi chrono common)
  (import (scheme base)
          (chibi chrono base) (chibi chrono time-zone)
          (chibi assert) (chibi locale) (chibi optional))
  (export chronology:gregorian chronology:gregorian-date
          chronology:julian-date chronology:time
          make-date make-datetime make-julian-date
          date? datetime? julian-date?
          date-year date-month date-day
          datetime-year datetime-month datetime-day
          datetime-hour datetime-minute datetime-second
          datetime-nanosecond datetime-zone
          make-time time? time-hour time-minute time-second time-nanosecond
          julian-date-year julian-date-month julian-date-day
          gregorian->instant instant->gregorian
          gregorian-date->instant instant->gregorian-date
          julian-date->instant instant->julian-date
          is-leap-year? is-julian-leap-year?
          leap-years-from-epoch-to
          day-of-week julian-day-of-week month-day-upper-bound
          day-of-year julian-day-of-year week-of-year julian-week-of-year
          week-of-month julian-week-of-month
          gregorian->julian-day-number julian->julian-day-number
          cumulative-days-to-start-of-month)
  (include "common.scm"))
