
(define-record-type VCalendar
  (make-vcalendar id version calscale method timezones events meta)
  vcalendar?
  (id vcalendar-id)
  (version vcalendar-version)
  (calscale vcalendar-calscale)
  (method vcalendar-method)
  (timezones vcalendar-timezones)
  (events vcalendar-events)
  (meta vcalendar-meta))

;; (define-record-type VRecurrence
;;   (%make-vrecurrence frequency interval count start end second minute hour day month month-day year-day week-number week-start)
;;   vrecurrence?
;;   (frequency vrecurrence-frequency vrecurrence-frequency-set!)
;;   (interval vrecurrence-interval vrecurrence-interval-set!)
;;   (count vrecurrence-count vrecurrence-count-set!)
;;   (start vrecurrence-start vrecurrence-start-set!)
;;   (end vrecurrence-end vrecurrence-end-set!)
;;   (second vrecurrence-second vrecurrence-second-set!)
;;   (minute vrecurrence-minute vrecurrence-minute-set!)
;;   (hour vrecurrence-hour vrecurrence-hour-set!)
;;   (day vrecurrence-day vrecurrence-day-set!)
;;   (month vrecurrence-month vrecurrence-month-set!)
;;   (month-day vrecurrence-month-day vrecurrence-month-day-set!)
;;   (year-day vrecurrence-year-day vrecurrence-year-day-set!)
;;   (week-number vrecurrence-week-number vrecurrence-week-number-set!)
;;   (week-start vrecurrence-week-start vrecurrence-week-start-set!))

(define-record-type TzOffset
  (make-tzoffset name offset-from offset-to start recurrence)
  tzoffset?
  (name tzoffset-name)
  (offset-from tzoffset-offset-from)
  (offset-to tzoffset-offset-to)
  (start tzoffset-start)
  (recurrence tzoffset-recurrence))

(define-record-type VTimezone
  (make-vtimezone id standard daylight)
  vtimezone?
  (id vtimezone-id)
  (standard vtimezone-standard)
  (daylight vtimezone-daylight))

(define-record-type Attendee
  (make-attendee type role status name meta)
  attendee?
  (type attendee-type)
  (role attendee-role)
  (status attendee-status)
  (name attendee-name)
  (meta attendee-meta))

(define-record-type VAlarm
  (make-valarm action description trigger)
  valarm?
  (action valarm-action)
  (description valarm-description)
  (trigger valarm-trigger))

(define-record-type VEvent
  (%make-vevent uid start end recur organizer attendees created modified location sequence status class transparency summary description alarm attach)
  vevent?
  (uid vevent-uid)
  (start vevent-start)
  (end vevent-end)
  (recur vevent-recur)
  (organizer vevent-organizer)
  (attendees vevent-attendees)
  (created vevent-created)
  (modified vevent-modified)
  (location vevent-location)
  (sequence vevent-sequence)
  (status vevent-status)
  (class vevent-class)
  (transparency vevent-transparency)
  (summary vevent-summary)
  (description vevent-description)
  (alarm vevent-alarm)
  (attach vevent-attach))

(define make-vevent
  (let ((count 0))
    (define (next-uid domain)
      (let ((uid (string-append "e" (number->string count) "@" domain)))
        (set! count (+ count 1))
        uid))
    (lambda args
      (let-keywords* args ((domain "chibi.nowhere")
                           (uid (next-uid domain))
                           (created (instant->temporal (current-second)))
                           (start #f)
                           (end #f)
                           (recur #f)
                           (organizer #f)
                           (attendees '())
                           (modified #f)
                           (location #f)
                           (sequence #f)
                           (status #f)
                           (class #f)
                           (transparency #f)
                           (summary #f)
                           (description #f)
                           (alarm #f)
                           (attach '()))
        (%make-vevent uid start end recur organizer attendees
                      created modified location sequence status
                      class transparency summary description
                      alarm attach)))))

(define (warn . msg)
  (for-each (lambda (s) (write-string s (current-error-port))) msg)
  (newline (current-error-port)))

(define (parse-date str)
  (case (string-length str)
    ((8)
     (string->temporal str '((fit0 4 year) (fit0 2 month) (fit0 2 day))))
    ((15)
     (string->temporal
      str
      '((fit0 4 year) (fit0 2 month) (fit0 2 day) "T"
        (fit0 2 hour) (fit0 2 minute) (fit0 2 second))))
    ((16)
     (string->temporal
      str
      '((fit0 4 year) (fit0 2 month) (fit0 2 day) "T"
        (fit0 2 hour) (fit0 2 minute) (fit0 2 second) "Z")))
    (else
     (warn "can't parse date: " str)
     #f)))

(define (format-date dt)
  (if (and (zero? (datetime-hour dt))
           (zero? (datetime-minute dt))
           (zero? (datetime-second dt)))
      (temporal->string
       dt '((fit0 4 year) "-" (fit0 2 month) "-" (fit0 2 day) " "
            (fit-right 3 day-of-week-name)))
      (temporal->string
       dt '((fit0 4 year) "-" (fit0 2 month) "-" (fit0 2 day) " "
            (fit-right 3 day-of-week-name) " "
            (fit0 2 hour) ":" (fit0 2 minute)))))

(define (string->downcase-symbol str)
  (string->symbol (string-downcase str)))

(define (read-space-wrapped-line in)
  (let ((line (read-line in)))
    (cond
     ((and (char? (peek-char in))
           (char-whitespace? (peek-char in)))
      (let lp ((ls (list (substring (read-line in) 1) line)))
        (if (char-whitespace? (peek-char in))
            (lp (cons (substring (read-line in) 1) ls))
            (string-concatenate-reverse ls))))
     (else
      line))))

(define (parse-params str)
  (map (lambda (x)
         (cons (string->symbol (car x))
               (if (pair? (cdr x)) (cadr x) "")))
       (remove null?
               (map (lambda (s) (string-split s "="))
                    (string-split str ";")))))

(define (read-ical-line . o)
  (let* ((in (if (pair? o) (car o) (current-input-port)))
         (str (read-space-wrapped-line in)))
    (cond
     ((eof-object? str)
      str)
     ((equal? str "")
      (read-ical-line in))
     (else
      (let* ((colon (string-index str #\:))
             (semi-colon (string-index str
                                       #\;
                                       (string-cursor-start str)
                                       colon))
             (name (string->symbol
                    (substring/cursors str
                                       (string-cursor-start str)
                                       semi-colon)))
             (params (if (string-cursor=? colon semi-colon)
                         '()
                         (parse-params
                          (substring/cursors str semi-colon colon))))
             (value (substring/cursors str
                                       (string-cursor-next str colon)
                                       (string-cursor-end str))))
        (list name params value))))))

(define (validate-begin type in)
  (let ((x (read-ical-line in)))
    (cond
     ((eof-object? x) (error "empty input"))
     ((and (eq? 'BEGIN (car x)) (equal? type (third x))))
     (else (error (string-append "invalid start of " type) x)))))

(define (read-ical-alarm . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (after-first-line? (and (pair? o) (pair? (cdr o)) (cadr o))))
    (unless after-first-line?
      (validate-begin "VALARM" in))
    (loop lp ((with action #f)
              (with description #f)
              (with trigger #f))
      (let ((line (read-ical-line in)))
        (if (eof-object? line)
            (error "end of input in valarm")
            (case (car line)
              ((BEGIN) (error "unknown subtype in valarm" line))
              ((END)
               (if (equal? "VALARM" (third line))
                   (make-valarm action description trigger)
                   (error "mismatched end in valarm" line)))
              ((ACTION)
               (lp (=> action (third line))))
              ((DESCRIPTION)
               (lp (=> description (third line))))
              ((TRIGGER)
               (lp (=> trigger (third line))))
              (else
               (if (string-prefix? "X-" (symbol->string (car line)))
                   (lp (=> meta (cons line meta)))
                   (error "unexpected line in valarm" line)))))))))

(define (read-ical-tzoffset type . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (after-first-line? (and (pair? o) (pair? (cdr o)) (cadr o))))
    (unless after-first-line?
      (validate-begin type in))
    (loop lp ((with name #f)
              (with meta '())
              (with offset-from #f)
              (with offset-to #f)
              (with start #f)
              (with recurrence #f))
      (let ((line (read-ical-line in)))
        (if (eof-object? line)
            (error "end of input in tzoffset" name)
            (case (car line)
              ((BEGIN) (error "unknown subtype in tzoffset" line))
              ((END)
               (if (equal? type (third line))
                   (make-tzoffset name offset-from offset-to start recurrence)
                   (error "mismatched end in tzoffset" line)))
              ((TZNAME)
               (lp (=> name (third line))))
              ((TZOFFSETFROM)
               (lp (=> offset-from (third line))))
              ((TZOFFSETTO)
               (lp (=> offset-to (third line))))
              ((DTSTART)
               (lp (=> start (third line))))
              ((RRULE)
               (lp (=> recurrence (second line))))
              (else
               (if (string-prefix? "X-" (symbol->string (car line)))
                   (lp (=> meta (cons line meta)))
                   (error (string-append "unexpected line in " type) line)
                 ))))))))

(define (read-ical-timezone . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (after-first-line? (and (pair? o) (pair? (cdr o)) (cadr o))))
    (unless after-first-line?
      (validate-begin "VTIMEZONE" in))
    (loop lp ((with id #f)
              (with meta '())
              (with standard #f)
              (with daylight #f))
      (let ((line (read-ical-line in)))
        (cond
         ((eof-object? line) (error "end of input in vtimezone" id))
         ((and (eq? 'END (car line)) (equal? "VTIMEZONE" (third line)))
          (make-vtimezone id standard daylight))
         ((eq? 'BEGIN (car line))
          (case (string->symbol (third line))
            ((STANDARD)
             (lp (=> standard (read-ical-tzoffset "STANDARD" in #t))))
            ((DAYLIGHT)
             (lp (=> daylight (read-ical-tzoffset "DAYLIGHT" in #t))))
            (else (error "unknown subtype in vtimezone" line))))
         (else
          (cond
           ((eq? 'TZID (car line))
            (lp (=> id (third line))))
           ((string-prefix? "X-" (symbol->string (car line)))
            (lp (=> meta (cons line meta))))
           (else
            (error "unexpected line in vtimezone" line)))))))))

(define (read-ical-event . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (after-first-line? (and (pair? o) (pair? (cdr o)) (cadr o))))
    (unless after-first-line?
      (validate-begin "VEVENT" in))
    (loop lp ((with uid #f)
              (with start #f)
              (with end #f)
              (with recur #f)
              (with organizer #f)
              (with attendees '())
              (with created #f)
              (with modified #f)
              (with location #f)
              (with sequence #f)
              (with status #f)
              (with class #f)
              (with transparency #f)
              (with summary #f)
              (with description #f)
              (with alarm #f)
              (with attach '())
              (with meta '()))
      (let ((line (read-ical-line in)))
        (if (eof-object? line)
            (error "end of input in vevent" uid)
            (case (car line)
              ((UID) (lp (=> uid (third line))))
              ((DTSTART) (lp (=> start (third line))))
              ((DTEND) (lp (=> end (third line))))
              ((DTSTAMP) (lp))
              ((ORGANIZER) (lp (=> organizer (third line))))
              ((ATTENDEE)
               (lp (=> attendees
                       (cons (cons (second line) (third line)) attendees))))
              ((CREATED) (lp (=> created (third line))))
              ((LAST-MODIFIED) (lp (=> modified (third line))))
              ((SUMMARY) (lp (=> summary (third line))))
              ((DESCRIPTION) (lp (=> description (third line))))
              ((LOCATION) (lp (=> location (third line))))
              ((SEQUENCE) (lp (=> sequence (string->number (third line)))))
              ((STATUS) (lp (=> status (third line))))
              ((TRANSP) (lp (=> transparency (third line))))
              ((CLASS) (lp (=> class (third line))))
              ((ATTACH) (lp (=> attach (cons (third line) attach))))
              ((RRULE) (lp (=> recur (parse-params (third line)))))
              ((BEGIN)
               (if (equal? "VALARM" (third line))
                   (lp (=> alarm (read-ical-alarm in #t)))
                   (error "unknown subtype in vevent" line)))
              ((END)
               (if (equal? "VEVENT" (third line))
                   (%make-vevent uid start end recur organizer attendees
                                 created modified location sequence status
                                 class transparency summary description
                                 alarm attach)
                   (error "unmatched end in vevent" line)))
              (else
               (if (string-prefix? "X-" (symbol->string (car line)))
                   (lp (=> meta (cons line meta)))
                   (error "unexpected line in vevent" line)))))))))

(define (read-ical . o)
  (let ((in (if (pair? o) (car o) (current-input-port)))
        (after-first-line? (and (pair? o) (pair? (cdr o)) (cadr o))))
    (unless after-first-line?
      (validate-begin "VCALENDAR" in))
    (loop lp ((with id #f)
              (with version 2.0)
              (with cal 'gregorian)
              (with method 'publish)
              (with tzs '())
              (with events '())
              (with meta '()))
      (let ((line (read-ical-line in)))
        (if (or (eof-object? line)
                (and (eq? 'END (car line))
                     (equal? "VCALENDAR" (third line))))
            (make-vcalendar id version cal method
                            (reverse tzs)
                            (reverse events)
                            (reverse meta))
            (case (car line)
              ((PRODID) (lp (=> id (third line))))
              ((VERSION) (lp (=> version (string->number (third line)))))
              ((CALSCALE) (lp (=> cal (string->downcase-symbol (third line)))))
              ((METHOD) (lp (=> method (string->downcase-symbol (third line)))))
              ((BEGIN)
               (case (string->symbol (third line))
                 ((VTIMEZONE)
                  (lp (=> tzs (cons (read-ical-timezone in #t) tzs))))
                 ((VEVENT)
                  (lp (=> events (cons (read-ical-event in #t) events))))
                 (else (error "unknown subtype in vcalendar" line))))
              ((END) (error "unexpected END" (third line)))
              (else
               (if (string-prefix? "X-" (symbol->string (car line)))
                   (lp (=> meta (cons line meta)))
                   (error "unexpected line in vcalendar" line)))))))))

(define (string->ical str)
  (let* ((in (open-input-string str))
         (res (read-ical in)))
    (close-input-port in)
    res))

(define (write-ical->org cal . o)
  (define (clean str)
    (regexp-replace-all
     '(: "<a href=\"" ($ (* (~ ("\"")))) "\"" (* (~ (">"))) ">"
         ($ (* (~ ("<")))) "</a>")
     (regexp-replace-all
      "\n*"
      (regexp-replace-all
       '"<br>"
       (regexp-replace-all
        '(: "\\" ($ any))
        str
        (lambda (m)
          (case (string-ref str (regexp-match-submatch-start m 1))
            ((#\n) "\n")
            ((#\r) "")
            ((#\t) "\t")
            (else (regexp-match-submatch m 1)))))
       "\n")
      "\n *")
     (lambda (m)
       (string-append "[[" (regexp-match-submatch m 1) "]["
                      (regexp-match-submatch m 2) "]]"))))
  (define (recur->org recur)
    (cond
     ((assq 'FREQ recur) =>
      (lambda (x)
        (case (string->symbol (cdr x))
          ((YEARLY) "+1y")
          ((MONTHLY) "+1m")
          ((DAILY) "+1d")
          (else ""))))
     (else "")))
  (let ((out (if (pair? o) (car o) (current-output-port))))
    ;; TODO: customize this
    (write-string "#+TITLE:       Calendar Title
#+AUTHOR:      My Name
#+EMAIL:       myaddress@mydomain.com

" out)
    (for-each
     (lambda (event)
       (guard (exn (else (warn "error writing event")
                         (print-exception exn)))
         (let ((start (parse-date (vevent-start event)))
               (end (and (vevent-end event)
                         (parse-date (vevent-end event)))))
           (write-string "* " out)
           (write-string (clean (vevent-summary event)) out)
           (newline out)
           (write-string " <" out)
           (write-string (format-date start) out)
           ;; preserve a single full-day event as one date
           (cond
            ((and end
                  (= (datetime-year start) (datetime-year end))
                  (= (datetime-month start) (datetime-month end))
                  (= (datetime-day start) (datetime-day end)))
             (write-string "-" out)
             (write-string
              (temporal->string end '((fit0 2 hour) ":" (fit0 2 minute)))
              out))
            ((and end (not (temporal>=? end (temporal-adjust start 'day 1))))
             (write-string ">--<" out)
             (write-string (format-date end) out)))
           (when (vevent-recur event)
             (write-string " " out)
             (write-string (recur->org (vevent-recur event)) out))
           (write-string ">\n" out)
           (when (vevent-description event)
             (write-string (clean (vevent-description event)) out))
           (newline out)
           (newline out))))
     (vcalendar-events cal))))

(define (ical->org cal)
  (let ((out (open-output-string)))
    (write-ical->org cal out)
    (get-output-string out)))

;; utility to read a ical on stdin and write org to stdout
(define (main args)
  (let* ((args (cdr args))
         (cal (if (pair? args)
                  (call-with-input-file (car args) read-ical)
                  (read-ical))))
    (write-string (ical->org cal))))
