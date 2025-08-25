
;;> Library grouping the four core chrono libraries:
;;>  \itemlist[
;;>    \item[\scheme{(chibi chrono base)} - core syntax and procedures]
;;>    \item[\scheme{(chibi chrono time-zone)} - generic time-zone procedures]
;;>    \item[\scheme{(chibi chrono format)} - generic formatting procedures]
;;>    \item[\scheme{(chibi chrono common)} - the gregorian and julian calendars]
;;>  ]
;;>
;;> In addition, provides the following convenience procedures, which
;;> are wrapped versions of the procedure of same name with a
;;> \scheme{chronology-} prefix, making the \var{chronology} argument
;;> optional and defaulting to \scheme{chronology:gregorian}:
;;>
;;>  \itemlist[
;;>    \item[\scheme{instant->temporal}]
;;>    \item[\scheme{list->temporal}]
;;>    \item[\scheme{alist->temporal}]
;;>    \item[\scheme{try-alist->temporal}]
;;>    \item[\scheme{string->temporal}]
;;>    \item[\scheme{temporal-formatter}]
;;>    \item[\scheme{temporal-parser}]
;;>  ]

(define-library (chibi chrono)
  (import (scheme base)
          (srfi 227)
          (chibi chrono base)
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
   temporal-min temporal-max
   temporal-chronology temporal-in-chronology temporal-fields
   temporal->instant instant->temporal
   temporal->list list->temporal
   temporal->alist alist->temporal try-alist->temporal
   ;; defining new chronologies
   define-chronology
   ;; chronology internals
   make-chronology chronology? chronology-known-field?
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
   unparseable-min-length unparseable-max-length)
  (begin
    ;; Wrappers providing a default chronology.
    (define (instant->temporal instant . o)
      (let-optionals o ((chronology chronology:gregorian))
        (chronology-instant->temporal instant chronology)))
    (define (list->temporal ls . o)
      (let-optionals o ((chronology chronology:gregorian))
        (chronology-list->temporal ls chronology)))
    (define (try-alist->temporal ls . o)
      (let-optionals o ((chronology chronology:gregorian)
                        (strict? #f))
        (chronology-try-alist->temporal ls chronology strict?)))
    (define (alist->temporal ls . o)
      (let-optionals o ((chronology chronology:gregorian)
                        (strict? #f))
        (chronology-alist->temporal ls chronology strict?)))
    ;;> (string->temporal str [fmt] [chronology] [locale strict? lookup])
    (define (string->temporal str . o)
      (cond
       ((null? o)
        (chronology-string->temporal str
                                     (chronology-format chronology:gregorian)
                                     chronology:gregorian))
       ((chronology? (car o))
        (apply chronology-string->temporal
               str
               (chronology-format (car o))
               (car o)
               (cdr o)))
       ((pair? (car o))
        (let-optionals o ((fmt #f) (chronology chronology:gregorian) . rest)
          (apply chronology-string->temporal
                 str
                 (or fmt (chronology-format chronology))
                 chronology
                 rest)))
       (else
        (error "string->temportal expected chronology or format, got" (car o))
        )))
    (define (temporal-formatter fmt . o)
      (let-optionals o ((chronology chronology:gregorian) . rest)
        (apply chronology-temporal-formatter fmt chronology rest)))
    (define (temporal-parser fmt . o)
      (let-optionals o ((chronology chronology:gregorian) . rest)
        (apply chronology-temporal-parser fmt chronology rest)))
    ))
