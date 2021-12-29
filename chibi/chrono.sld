
;;> Library grouping the four most common chrono libraries.

(define-library (chibi chrono)
  (import (chibi chrono base)
          (chibi chrono common)
          (chibi chrono format)
          (chibi chrono time-zone))
  (export
   ;; base
   ;; instants
   current-nanosecond
   ;; temporals
   temporal? temporal-ref temporal-update temporal-adjust
   temporal=? temporal<? temporal<=? temporal>? temporal>=?
   temporal-chronology temporal-in-chronology temporal-fields
   temporal->instant instant->temporal
   temporal->list list->temporal
   temporal->alist alist->temporal try-alist->temporal
   ;; defining new chronologies
   define-chronology default-chronology
   ;; chronology internals
   make-chronology chronology?
   chronology-name chronology-fields chronology-virtual
   chronology-constructor chronology-to-instant chronology-from-instant
   ;; field internals
   make-chrono-field chrono-field?
   chrono-field-name chrono-field-getter
   chrono-field-lb chrono-field-ub chrono-field-get-lb chrono-field-get-ub
   chrono-field-updater chrono-field-adjuster
   chrono-field-upper-bound chrono-field-lower-bound
   ;; time-zones
   ;; temporals with time-zones
   temporal-offset temporal-in-utc temporal-in-time-zone
   ;; time-zones
   time-zone? make-time-zone time-zone-name
   time-zone-infos time-zone-info time-zone-offset
   string->time-zone
   ;; common time-zones
   time-zone:utc
   ;; common
   chronology:gregorian chronology:gregorian-date
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
   day-of-week julian-day-of-week
   gregorian->julian-day-number julian->julian-day-number
   cumulative-days-to-start-of-month
   ;; format
   temporal->string string->temporal
   temporal-parser temporal-formatter
   temporal-macro? make-temporal-macro
   unparseable? make-unparseable
   unparseable-parse unparseable-unparse
   unparseable-min-length unparseable-max-length))
