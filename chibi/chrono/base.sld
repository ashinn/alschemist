
(define-library (chibi chrono base)
  (import (scheme base) (scheme hash-table) (scheme list) (scheme time)
          (srfi 99 records inspection)
          (chibi assert) (chibi optional))
  (export
   ;; instants
   current-nanosecond
   ;; temporals
   temporal? temporal-ref temporal-update temporal-adjust
   temporal=? temporal<? temporal<=? temporal>? temporal>=?
   temporal-min temporal-max
   temporal-chronology temporal-in-chronology temporal-fields
   temporal->instant chronology-instant->temporal
   temporal->list chronology-list->temporal
   temporal->alist chronology-alist->temporal chronology-try-alist->temporal
   make-duration duration? duration-field-counts duration-chronology
   duration->alist temporal-add-duration
   ;; defining new chronologies
   define-chronology
   ;; chronology internals
   make-chronology chronology? chronology-known-field?
   chronology-explicit-field? chronology-virtual-field?
   chronology-field<? chronology-field>?
   chronology-name chronology-fields chronology-virtual chronology-durations
   chronology-constructor chronology-to-instant chronology-from-instant
   chronology-format chronology-messages
   ;; field internals
   make-chrono-field chrono-field?
   chrono-field-name chrono-field-getter
   chrono-field-lb chrono-field-ub chrono-field-get-lb chrono-field-get-ub
   chrono-field-updater chrono-field-adjuster chrono-field-default
   chrono-field-upper-bound chrono-field-lower-bound
   virtual-field? virtual-field-name
   virtual-field-getter virtual-field-granularity)
  (include "base.scm"))
