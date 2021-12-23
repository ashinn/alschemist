
(define-library (chibi chrono time-zone)
  (import (scheme base) (chibi assert) (chibi chrono base)
          (chibi locale) (chibi optional))
  (export
   ;; temporals with time-zones
   temporal-offset temporal-in-utc temporal-in-time-zone
   ;; time-zones
   time-zone? make-time-zone time-zone-name
   time-zone-offset time-zone-get-additional-offset
   ;; common time-zones
   time-zone:utc time-zone:asia/tokyo time-zone:america/new-york)
  (include "time-zone.scm"))
