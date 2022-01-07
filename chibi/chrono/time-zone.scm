
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types

;; A rule for applying an additional offset to a time-zone as for
;; daylight savings time.
(define-record-type Tz-Rule
  (make-tz-rule from to month day-rule time save letter)
  tz-rule?
  (from tz-rule-from)
  (to tz-rule-to)
  (month tz-rule-month)
  (day-rule tz-rule-day-rule)
  (time tz-rule-time)
  (save tz-rule-save)
  (letter tz-rule-letter))

;; A range of time in which a given base offset and optional rules are
;; active for a time-zone.  For a given date and time only one info is
;; active.  Within the info multiple of the rules may be active,
;; although only one will be applied.
(define-record-type Tz-Info
  (%make-tz-info offset format until rule-set get-save)
  tz-info?
  (offset tz-info-offset)   ;; (hours [minutes [seconds]])
  (format tz-info-format)
  (until tz-info-until)
  (rule-set tz-info-rule-set)
  (get-save tz-info-get-save))

(define (make-tz-info offset format . o)
  (let-optionals* o ((until +inf.0)
                     (rule-set '())
                     (get-save #f))
    (%make-tz-info offset format until rule-set get-save)))

;; A time-zone consists of an official name and a an ordered vector of
;; historical Tz-Infos.
(define-record-type Time-Zone
  (make-time-zone name infos)
  time-zone?
  (name time-zone-name)
  (infos time-zone-infos))

(define time-zone:utc
  (make-time-zone "Etc/UTC" (vector (make-tz-info '(0) "UTC"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities

;; Add the values from two lists representing the same ordered time
;; parts.  Conceptually (map + ls1 ls2), but pads with zeros if the
;; lists aren't the same length.
(define (add-time-parts ls1 ls2)
  (let lp ((ls1 ls1) (ls2 ls2) (res '()))
    (cond
     ((null? ls1) (append (reverse res) ls2))
     ((null? ls2) (append (reverse res) ls1))
     (else (lp (cdr ls1) (cdr ls2) (cons (+ (car ls1) (car ls2)) res))))))

(define date-time-fields
  '(year month day hour minute second nanosecond))
(define time-fields
  '(hour minute second nanosecond))

;; Returns #t iff temporal t is strictly before the given list of
;; field values in descending order (default from year).
(define (temporal/list< t ls . o)
  (let lp ((ls ls)
           (fields (if (pair? o) (car o) date-time-fields)))
    (cond
     ((null? ls)
      (not (any (lambda (field) (positive? (temporal-ref t field))) fields)))
     ((null? fields)
      #f)
     (else
      (let ((value (temporal-ref t (car fields))))
        (cond
         ((< value (car ls)))
         ((= value (car ls)) (lp (cdr ls) (cdr fields)))
         (else #f)))))))

;; Returns #t iff temporal t is strictly after the given list of
;; field values in descending order (default from year).
(define (temporal/list> t ls . o)
  (let lp ((ls ls)
           (fields (if (pair? o) (car o) date-time-fields)))
    (cond
     ((null? ls)
      (any (lambda (field) (positive? (temporal-ref t field))) fields))
     ((null? fields)
      #f)
     (else
      (let ((value (temporal-ref t (car fields))))
        (cond
         ((> value (car ls)))
         ((= value (car ls)) (lp (cdr ls) (cdr fields)))
         (else #f)))))))

;; Returns #t iff temporal t is on or after the given list of
;; field values in descending order (default from year).
(define (temporal/list>= t ls . o)
  (let lp ((ls ls)
           (fields (if (pair? o) (car o) date-time-fields)))
    (cond
     ((null? ls)
      #t)
     ((null? fields)
      #t)
     (else
      (let ((value (temporal-ref t (car fields))))
        (cond
         ((> value (car ls)))
         ((= value (car ls)) (lp (cdr ls) (cdr fields)))
         (else #f)))))))

(define (resolve-rule-day t rule)
  (define (is-leap-year? year)
    (and (zero? (modulo year 4))
         (not (memv (modulo year 400) '(100 200 300)))))
  (define (month-day-upper-bound year month)
    (case month
      ((2) (if (is-leap-year? year) 29 28))
      ((4 6 9 11) 30)
      (else 31)))
  (let ((day-rule (tz-rule-day-rule rule)))
    (cond
     ((number? day-rule)
      day-rule)
     ((and (pair? day-rule) (eq? '>= (car day-rule)))
      (let* ((to-dow (second day-rule))
             (min-day (third day-rule))
             (dow (temporal-ref t 'day-of-week))
             (dow-of-1st (modulo (- dow (temporal-ref t 'day) -1) 7))
             (first-dow (+ 1 (modulo (- (+ to-dow 7) dow-of-1st) 7))))
        (if (>= first-dow min-day)
            first-dow
            (+ first-dow (* 7 (quotient (- min-day first-dow -6) 7))))))
     ((and (pair? day-rule) (eq? 'last (car day-rule)))
      (let* ((to-dow (second day-rule))
             (dow (temporal-ref t 'day-of-week))
             (dow-of-1st (modulo (- dow (temporal-ref t 'day) -1) 7))
             (first-dow (+ 1 (modulo (- (+ to-dow 7) dow-of-1st) 7)))
             (fourth-dow (+ first-dow (* 7 3)))
             (fifth-dow (+ first-dow (* 7 4))))
        ;; TODO: use a leap-year virtual field
        (if (> fifth-dow (month-day-upper-bound (temporal-ref t 'year)
                                                (temporal-ref t 'month)))
            fourth-dow
            fifth-dow)))
     (else
      (error "unknown day-rule" rule)))))

(define (temporal/rule> t rule)
  (let ((month (temporal-ref t 'month)))
    (cond
     ((> month (tz-rule-month rule)))
     ((= month (tz-rule-month rule))
      (let ((day (temporal-ref t 'day))
            (dst-day (resolve-rule-day t rule)))
        (cond ((> day dst-day) (values 0 0))
              ((= day dst-day)
               (temporal/list>= t (tz-rule-time rule) time-fields))
              (else #f))))
     (else #f))))

(define (temporal/rule= t rule)
  (and (= (temporal-ref t 'month) (tz-rule-month rule))
       (= (temporal-ref t 'day) (resolve-rule-day t rule))))

(define (temporal-adjust-list t fields values)
  (if (or (null? fields) (null? values))
      t
      (temporal-adjust-list (temporal-adjust t (car fields) (car values))
                            (cdr values)
                            (cdr fields))))

;; info at the given temporal
(define (time-zone-info tz t)
  (let ((vec (time-zone-infos tz)))
    ;; We just walk backwards instead of binary searching because in
    ;; practice these are small vectors and most usage is for the
    ;; latest rule.
    (let lp ((i (- (vector-length vec) 1)))
      (let ((info (vector-ref vec i)))
        (if (or (zero? i)
                (not (pair? (tz-info-until info)))
                (temporal/list>= t (tz-info-until info)))
            info
            (lp (- i 1)))))))

;;> Base offset at the given temporal, ignoring DST rules.
(define (time-zone-offset tz t)
  (tz-info-offset (time-zone-info tz t)))

;;> Full offset including DST rules at the given temporal.
(define (time-zone-offset+save tz t)
  (let ((info (time-zone-info tz t)))
    (cond
     ((tz-info-get-save info)
      => (lambda (get-save)
           (let-values (((save fold) (get-save t)))
             (add-time-parts (tz-info-offset info) save))))
     (else
      (tz-info-offset info)))))

;; alist of ((name . target-or-time-zone) ...)
;; where target is a string indicating a different name to lookup
(define tzdb #f)

(define (make-get-save rule-set)
  (lambda (t)
    (let lp ((i (- (vector-length rule-set) 1))
             (end #f))
      (cond
       ((and (>= i 0)
             (<= (tz-rule-from (vector-ref rule-set i))
                 (temporal-ref t 'year)
                 (tz-rule-to (vector-ref rule-set i))))
        (lp (- i 1) (or end i)))
       (end
        ;; Rules [i+1, end] are active for this year.
        ;; We collect all active rules and then run through them in forward,
        ;; because the conversion times are defined in terms of the adjusted
        ;; time, not the base offset.
        (let lp ((t2 t) (j (+ i 1)) (save '(0)) (fold 0))
          (if (and (<= j end) (temporal/rule> t2 (vector-ref rule-set j)))
              (let* ((rule (vector-ref rule-set j))
                     (jump-time (add-time-parts (tz-rule-time rule) save)))
                (lp (temporal-adjust-list t
                                          time-fields
                                          (tz-rule-save rule))
                    (+ j 1)
                    (tz-rule-save rule)
                    (if (and (temporal/list< t2 jump-time time-fields)
                             (temporal/list> t2 (tz-rule-time rule) time-fields)
                             (temporal/rule= t2 rule))
                        1
                        0)))
              (values save fold))))
       (else
        ;; no matches
        (values '(0) 0))))))

(define (tzdb-compile sexp)
  (define (compile-rule-set rule-set)
    (cons (car rule-set)
          (list->vector
           (map (lambda (rule) (apply make-tz-rule rule))
                (cdr rule-set)))))
  (let ((zones (cond ((assq 'zones sexp) => cdr) (else '())))
        (rule-sets (map compile-rule-set
                        (cond ((assq 'rules sexp) => cdr) (else '()))))
        (links (cond ((assq 'links sexp) => cdr) (else '()))))
    (define (compile-zone zone)
      (let ((infos
             (list->vector
              (map (lambda (x)
                     (let-optionals x (offset rule-name format (until +inf.0))
                       (let ((rule-set
                              (and (not (eq? '- rule-name))
                                   (cond
                                    ((assq rule-name rule-sets) => cdr)
                                    (else "error unknown rule" rule-name)))))
                         (make-tz-info offset format until rule-set
                                       (and rule-set (make-get-save rule-set))
                                       ))))
                   (cdr zone)))))
        (cons (car zone) (make-time-zone (car zone) infos))))
    (append (map compile-zone zones) links)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API

(define (offset->time-zone hours . o)
  (let* ((minutes (if (pair? o) (car o) 0))
         (name (string-append (number->string hours) ":"
                              (number->string minutes))))
    (make-time-zone name (vector (make-tz-info (list hours minutes) name)))))

;; x is a string or list of integers
(define (abbrev->time-zone name x)
  (if (string? x)
      (string->time-zone x)
      (make-time-zone name
                      (vector
                       (make-tz-info (if (number? x) (list x) x) name)))))

;;> Returns the time-zone with the given name, or \scheme{#f} if not a
;;> known name.  Tries to load from the tzdb data in /usr/share/zoneinfo.
;;> Accepts tzdb names as well as common abbreviations and +/-hh:mm format.
(define (string->time-zone name . o)
  (let ((locale (if (pair? o) (car o) locale:root)))
    (unless tzdb
      (guard (exn (else
                   (write-string ";; ERROR: couldn't load tzdb\n"
                                 (current-error-port))
                   (set! tzdb `(("Etc/UTC" . ,time-zone:utc)))))
        ;; TODO: binary tzfile support
        (set! tzdb
              (tzdb-compile
               (tzdb-parse-file "/usr/share/zoneinfo/tzdata.zi")))))
   (let get ((x name) (seen '()))
     (cond
      ((assoc x tzdb)
       => (lambda (cell)
            (if (string? (cdr cell))
                (if (member (cdr cell) seen)
                    (error "cycle in time-zone aliases" cell seen)
                    (get (cdr cell) (cons x seen)))
                (cdr cell))))
      ((equal? "" x)
       #f)
      ((and (= 1 (string-length x))
            (char-alphabetic? (string-ref x 0))
            (char-upper-case? (string-ref x 0)))
       (let* ((hours (+ 1 (- (char->integer (string-ref x 0))
                             (char->integer #\A))))
              (hours (if (> hours 9) (- hours 1) hours))  ;; no J
              (hours (if (> hours 12) (- 12 hours) hours))
              (hours (if (< hours -12) 0 hours)))
         (offset->time-zone hours)))
      ((memv (string-ref x 0) '(#\+ #\-))
       (case (string-length x)
         ((3) (cond ((string->number x) => offset->time-zone)))
         ((6)
          (and (char=? #\: (string-ref x 3))
               (let ((hours (string->number (substring x 0 3)))
                     (minutes (string->number (substring x 4))))
                 (and hours minutes (offset->time-zone hours minutes)))))
         (else #f)))
      (else
       (abbrev->time-zone
        x
        (case (string->symbol x)
          ((BIT IDLW) -12)
          ((NUT) -11)
          ((CKT HST SDT TAHT) -10)
          ((MART MIT) '(-09 30))
          ((AKST GAMT GIT HDT) -09)
          ((AKDT CIST PST) -08)
          ((PDT) -07)
          ((CT) "America/Chicago")
          ((MDT EAST GALT) -06)
          ((COT EASST EST PET) -05)
          ((ET) "America/New_York")
          ((AST BOT CLT COST EDT FKT GYT PYT VET) -04)
          ((NST NT) '(-03 30))
          ((ADT AMST ART BRT CLST FKST GFT PMST PYST ROTT SRT UYT WGT) -03)
          ((NDT) '(-02 30))
          ((BRST FNT PMDT UYST WGST) -02)
          ((AZOT CVT EGT) -01)
          ((AZOST EGST GMT UTC WET) 0)
          ((CET DFT MET WAT WEST) +01)
          ((CAT CEST EET HAEC KALT MEST SAST WAST) +02)
          ((EAT EEST FET IDT IOT MSK SYOT TRT) +03)
          ((IRST) '(+03 30))
          ((AZT GET MUT RET SAMT SCT VOLT) +04)
          ((AFT) '(+04 30))
          ((IRDT) '(+04 30))
          ((AQTT HMT MAWT MVT ORAT PKT TFT TJT TMT UZT YEKT) +05)
          ((SLST) '(+05 30))
          ((NPT) '(+05 45))
          ((ALMT BIOT BTT KGT OMST VOST) +06)
          ((CCT MMT) '(+06 30))
          ((CXT DAVT HOVT ICT KRAT NOVT THA WIB) +07)
          ((AWST BNT CHOT HKT HOVST IRKT PHST PHT SGT ULAT WITA WST) +08)
          ((ACWST CWST) '(+08 45))
          ((CHOST JST KST MYT PWT ULAST WIT TLT YAKT) +09)
          ((ACST) '(+09 30))
          ((AEST AET CHST CHUT DDUT PGT VLAT) +10)
          ((ACDT LHST) '(+10 30))
          ((AEDT KOST MIST NCT NFT PONT SAKT SBT SRET VUT) +11)
          ((ANAT FJT GILT MAGT MHT NZST PETT TVT WAKT) +12)
          ((CHAST) '(+12 45))
          ((NZDT PHOT TKT TOT) +13)
          ((CHADT) '(+13 45))
          ((LINT) +14)
          ((ACT) (if (or (eq? 'BR (locale-region locale))
                         (eq? 'pt (locale-language locale)))
                     -05
                     +08))
          ((AMT) (if (or (eq? 'AM (locale-region locale))
                         (memq (locale-language locale) '(hy hye arm)))
                     '(+04)
                     '(-04)))
          ((AST) (if (or (eq? 'ar (locale-language locale))
                         (memq (locale-region locale) '(BH IQ KW QA SA YE)))
                     '(+03)
                     '(-04))
           +03)
          ((BST) (cond ((eq? 'PG (locale-region locale)) +11)
                       ((eq? 'BD (locale-region locale)) +06)
                       (else +01)))
          ((CDT) (if (eq? 'CU (locale-region locale)) -04 -05))
          ((CST) (case (locale-region locale) ((ZH) +08) ((CU) -05) (else -06)))
          ((ECT) (if (eq? 'EC (locale-region locale)) -05 -04))
          ((GST) (if (eq? 'GS (locale-region locale)) -02 +04))
          ((IST) (cond ((eq? 'IL (locale-region locale)) +02)
                       ((eq? 'IN (locale-region locale)) '(+05 30))
                       (else +01)))
          ((MST) (if (eq? 'MY (locale-region locale)) +08 -07))
          ((SST) (if (eq? 'WS (locale-region locale)) -11 +08))
          (else #f))))))))

;;> Returns the offset from UTC at the given time.
(define (temporal-offset t)
  (time-zone-offset+save (temporal-ref t 'time-zone) t))

;;> Returns the temporal in UTC, with the time adjusted accordingly.
(define (temporal-in-utc t)
  (if (eq? time-zone:utc (temporal-ref t 'time-zone))
      t
      (temporal-adjust-list (temporal-update t 'time-zone time-zone:utc)
                            time-fields
                            (map - (temporal-offset t)))))

;;> Returns the temporal in the given time-zone, with the time
;;> adjusted accordingly.
(define (temporal-in-time-zone t tz)
  (if (eq? tz (temporal-ref t 'time-zone))
      t
      (let* ((t (temporal-in-utc t))
             (offset (time-zone-offset tz t))
             (t2 (temporal-adjust-list
                  (temporal-update t 'time-zone tz)
                  '(hour minute second)
                  offset))
             (info (time-zone-info tz t2)))
        (cond
         ((tz-info-get-save info)
          => (lambda (get-save)
               (let-values (((save fold) (get-save t2)))
                 (temporal-update
                  (temporal-adjust-list t2 time-fields save)
                  'fold fold))))
         (else
          t2)))))
