
(define-library (chibi ical-test)
  (import (scheme base) (chibi ical) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "ical")
      (let ((cal (make-vcalendar
                  "-//Google Inc//Google Calendar 70.9054//EN"
                  2.0
                  'gregorian
                  'publish
                  (list
                   (make-vtimezone
                    "America/New_York"
                    (make-tzoffset "EST" "-0400" "-0500" "19701101T020000" '())
                    (make-tzoffset "EDT" "-0500" "-0400" "19700308T020000" '()))
                   )
                  (list
                   (make-vevent
                    'uid: "344blplerufgk2kq767clhsk0s@test.com"
                    'start: "20200622T070000Z"
                    'end: "20200622T072000Z"
                    'created: "20200619T051330Z"
                    'modified: "20200619T055240Z"
                    'organizer: "mailto:bob@test.com"
                    'location: ""
                    'attendees: '((((CUTYPE . "INDIVIDUAL")
                                    (ROLE . "REQ-PARTICIPANT")
                                    (PARTSTAT . "ACCEPTED")
                                    (CN . "Bob")
                                    (X-NUM-GUESTS . "0"))
                                   . "mailto:bob@test.com")
                                  (((CUTYPE . "INDIVIDUAL")
                                    (ROLE . "REQ-PARTICIPANT")
                                    (PARTSTAT . "ACCEPTED")
                                    (CN . "alice@test.com")
                                    (X-NUM-GUESTS . "0"))
                                   . "mailto:alice@test.com"))
                    'transparency: "OPAQUE"
                    'status: "CONFIRMED"
                    'sequence: 0
                    'summary: "summary..."
                    'description: "description...\\non multiple\\nlines.\\n"
                    ))
                  '((X-WR-CALNAME () "alice@test.com")
                    (X-WR-TIMEZONE () "America/New_York")))))
        (test cal
           (string->ical
            "BEGIN:VCALENDAR
PRODID:-//Google Inc//Google Calendar 70.9054//EN
VERSION:2.0
CALSCALE:GREGORIAN
METHOD:PUBLISH
X-WR-CALNAME:alice@test.com
X-WR-TIMEZONE:America/New_York
BEGIN:VTIMEZONE
TZID:America/New_York
X-LIC-LOCATION:America/New_York
BEGIN:DAYLIGHT
TZOFFSETFROM:-0500
TZOFFSETTO:-0400
TZNAME:EDT
DTSTART:19700308T020000
RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=2SU
END:DAYLIGHT
BEGIN:STANDARD
TZOFFSETFROM:-0400
TZOFFSETTO:-0500
TZNAME:EST
DTSTART:19701101T020000
RRULE:FREQ=YEARLY;BYMONTH=11;BYDAY=1SU
END:STANDARD
END:VTIMEZONE
BEGIN:VEVENT
DTSTART:20200622T070000Z
DTEND:20200622T072000Z
DTSTAMP:20200629T021444Z
ORGANIZER;CN=Bob:mailto:bob@test.com
UID:344blplerufgk2kq767clhsk0s@test.com
ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=alice
 @test.com;X-NUM-GUESTS=0:mailto:alice@test.com
ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;CN=Bob
 ;X-NUM-GUESTS=0:mailto:bob@test.com
CREATED:20200619T051330Z
DESCRIPTION:description...\\n
 on multiple\\n
 lines.\\n
LAST-MODIFIED:20200619T055240Z
LOCATION:
SEQUENCE:0
STATUS:CONFIRMED
SUMMARY:summary...
TRANSP:OPAQUE
END:VEVENT
END:VCALENDAR
"))
        (test "#+TITLE:       Calendar Title
#+AUTHOR:      My Name
#+EMAIL:       myaddress@mydomain.com

* summary...
 <2020-06-22 Mon 07:00-07:20>
description...
on multiple
lines.


"
            (ical->org cal)))
      (test-end))))
