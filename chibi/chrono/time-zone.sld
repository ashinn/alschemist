
(define-library (chibi chrono time-zone)
  (import (scheme base) (scheme char) (scheme list)
          (chibi assert) (chibi locale) (chibi optional)
          (chibi chrono base) (chibi chrono tzdb))
  (export
   ;; temporals with time-zones
   temporal-offset temporal-in-utc temporal-in-time-zone
   ;; time-zones
   time-zone? make-time-zone time-zone-name
   time-zone-infos time-zone-info
   time-zone-offset time-zone-offset+save
   string->time-zone offset->time-zone
   ;; low-level
   tz-info-offset tz-info-format tz-info-until
   tz-info-rule-set tz-info-get-save
   tz-rule-from tz-rule-to tz-rule-month tz-rule-day-rule
   tz-rule-time tz-rule-save tz-rule-letter
   ;; standard time-zones
   time-zone:utc)
  (include "time-zone.scm"))
