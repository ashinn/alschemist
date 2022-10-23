
(define-library (chibi ical)
  (import (scheme base) (scheme char) (scheme file)
          (scheme list) (scheme regex) (scheme time)
          (srfi 130)
          (chibi loop) (chibi optional) (chibi chrono))
  (cond-expand
   (chibi
    (import (only (chibi) print-exception)))
   (else
    (import (scheme write))
    (begin
      (define (print-exception exn)
        (write exn (current-error-port))
        (newline (current-error-port))))))
  (export
   make-vcalendar vcalendar?
   vcalendar-calscale vcalendar-id vcalendar-method vcalendar-version
   vcalendar-events vcalendar-meta vcalendar-timezones
   make-tzoffset tzoffset? tzoffset-name tzoffset-offset-from
   tzoffset-offset-to tzoffset-recurrence tzoffset-start
   make-vtimezone vtimezone? vtimezone-daylight vtimezone-id
   vtimezone-standard
   make-attendee attendee? attendee-name attendee-role attendee-status
   attendee-type attendee-meta
   make-valarm valarm? valarm-action valarm-description valarm-trigger
   make-vevent vevent? vevent-alarm vevent-attendees vevent-created
   vevent-end vevent-sequence vevent-modified vevent-recur vevent-summary
   vevent-uid vevent-attach vevent-status vevent-class vevent-start
   vevent-transparency vevent-description vevent-location vevent-organizer
   read-ical string->ical ical->org write-ical->org main)
  (include "ical.scm"))
