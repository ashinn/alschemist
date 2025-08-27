
(define-library (chibi chrono schedule)
  (import (scheme base)
          (scheme time)
          (only (srfi 125) hash)
          (srfi 128)
          (srfi 146)
          (srfi 227)
          (chibi bench)
          (chibi chrono base)
          (chibi chrono recurrence))
  (export make-schedule schedule?
          schedule-name schedule-what schedule-when schedule-next
          schedule=? schedule<?
          make-time-table time-table time-table? time-table-empty?
          time-table-schedules time-table-schedules-set!
          time-table-add time-table-add! time-table-next
          time-table-project time-table-run
          list->time-table
          recurrence->schedule make-one-time-schedule
          make-daily-schedule make-weekly-schedule
          make-monthly-schedule make-quarterly-schedule
          make-yearly-schedule)
  (include "schedule.scm"))
