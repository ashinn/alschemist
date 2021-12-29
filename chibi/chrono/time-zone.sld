
(define-library (chibi chrono time-zone)
  (import (scheme base) (scheme list)
          (chibi assert) (chibi locale) (chibi optional)
          (chibi chrono base) (chibi chrono tzdb))
  (export
   ;; temporals with time-zones
   temporal-offset temporal-in-utc temporal-in-time-zone
   ;; time-zones
   time-zone? make-time-zone time-zone-name
   time-zone-infos time-zone-info
   time-zone-offset time-zone-offset+save
   string->time-zone
   time-zone:utc)
  (include "time-zone.scm"))
