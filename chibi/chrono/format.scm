
(define-record-type Temporal-Macro
  (make-temporal-macro proc)
  temporal-macro?
  (proc temporal-macro-proc))

(define-record-type Virtual-Field
  (make-virtual-field name proc)
  virtual-field?
  (name virtual-field-name)
  (proc virtual-field-proc))

(define-record-type Unparseable
  (%make-unparseable unparse parse min-length max-length kons)
  unparseable?
  (unparse unparseable-unparse)
  (parse unparseable-parse)
  (min-length unparseable-min-length)
  (max-length unparseable-max-length)
  (kons unparseable-kons))

(define (make-unparseable unparse . o)
  (let-optionals o ((parse #f)
                    (min-length (lambda (ls) 0))
                    (max-length (lambda (ls) +inf.0))
                    (kons #f))
    (%make-unparseable unparse parse min-length max-length kons)))

(define (unparseable-fixed-length? p args)
  (and (unparseable-min-length p)
       (unparseable-max-length p)
       (= (apply (unparseable-min-length p) args)
          (apply (unparseable-max-length p) args))))

;; string/number: itself
;; symbol:
;;   field-name: numeric field value
;;   virtual-field-name: numeric value
;;   alias: recurse
;; (ref vec x): vector-ref given numeric value of x
;; (pad n x [ch]): left pad format x to n chars
;; (pad-right n x [ch]): right pad format x to n chars
;; (pad0 n x): short for (pad n x #\0)
;; (pad0-right n x [ch]): short for (pad-right n x #\0)
;; (fit[0][-right] n x [ch]): pads or truncates as needed
;; (ordinal x): locale-specific ordinal enum (en = st: 0, nd: 1, rd: 2, th: 3)
;; (ref #("st" "nd" "rd" "th") (ordinal x))
;; (nth x): short for (ref nth-names (ordinal x))
;; (optional x): format x as usual, but don't fail if it's missing on parse
;; (or x y ...): format just x, on parsing try alternatives
;; am/pm: (ref am/pm-names (modulo hour 12))
;; time-zone: tzdb name, e.g. America/New_York
;; time-zone-offset: +hhmm offset
;; time-zone-abbrev: ambiguous abbrev like EST, CDT, etc.
;; time-zone-military: single letter military offset
;;
;; for x in date, date-time and time:
;;   x/long, x, x/short

(define (chronology-get-field chrono field)
  (let lp ((ls (chronology-fields chrono)))
    (and (pair? ls)
         (if (eq? field (chrono-field-name (car ls)))
             (car ls)
             (lp (cdr ls))))))

(define (->string x)
  (if (string? x)
      x
      (let ((out (open-output-string)))
        (write x out)
        (get-output-string out))))

(define (temporal-pad n x . o)
  (let* ((ch (if (pair? o) (car o) #\space))
         (str (->string x))
         (len (string-length str)))
    (if (< len n)
        (string-append (make-string (- n len) ch) str)
        str)))

(define (temporal-pad-right n x . o)
  (let* ((ch (if (pair? o) (car o) #\space))
         (str (->string x))
         (len (string-length str)))
    (if (< len n)
        (string-append str (make-string (- n len) ch))
        str)))

(define (string-truncate n str)
  (let ((len (string-length str)))
    (if (> len n) (substring str (- len n) len) str)))

(define (string-truncate-right n str)
  (let ((len (string-length str)))
    (if (> len n) (substring str 0 n) str)))

(define (temporal-fit n x . o)
  (string-truncate n (apply temporal-pad n x o)))

(define (temporal-fit-right n x . o)
  (string-truncate-right n (apply temporal-pad-right n x o)))

(define (temporal-unpad n x . o)
  (let ((ch (if (pair? o) (car o) #\space)))
    (lambda (str sc kons pass fail)
      (let ((sc2 (string-cursor-forward str sc n)))
        (if (and (string-cursor<? sc sc2)
                 (string-cursor<=? sc2 (string-cursor-end str)))
            (let* ((substr (substring/cursors str sc sc2))
                   (end (string-cursor-end substr))
                   (end-1 (string-cursor-prev substr end)))
              ;; strip off all but the last char matching the pad
              (let lp ((sc3 (string-cursor-start substr )))
                (if (and (string-cursor<? sc3 end-1)
                         (eqv? ch (string-ref/cursor substr sc3)))
                    (lp (string-cursor-next substr sc3))
                    (let ((res (substring/cursors substr sc3 end)))
                      (pass res sc2 fail)))))
            (fail "insufficient input to unpad" str))))))

(define (temporal-unpad-right n x . o)
  (let ((ch (if (pair? o) (car o) #\space)))
    (lambda (str sc kons pass fail)
      (let ((sc2 (string-cursor-forward str sc n)))
        (if (and (string-cursor<? sc sc2)
                 (string-cursor<=? sc2 (string-cursor-end str)))
            (let* ((substr (substring/cursors str sc sc2))
                   (start (string-cursor-start substr))
                   (start+1 (string-cursor-next substr start)))
              ;; strip off all but the first char matching the pad
              (let lp ((sc3 (string-cursor-end substr)))
                (let ((sc4 (and (string-cursor>? sc3 start+1)
                                (string-cursor-prev substr sc3))))
                  (if (and sc4 (eqv? ch (string-ref/cursor substr sc4)))
                      (lp sc4)
                      (let ((res (substring/cursors substr start sc3)))
                        (pass res sc2 fail))))))
            (fail "insufficient input to unpad" str))))))

(define (temporal-unpad-min-length n x . o)
  n)

(define (temporal-unpad-max-length n x . o)
  +inf.0)

(define (temporal-unfit n x . o)
  (let ((unpad (apply temporal-unpad n x o)))
    (if (and (pair? x) (eq? temporal-unparseable:ref (car x)))
        (let ((parse-ref (alternate-parser
                          (vector-map (lambda (s) (string-take-right s n))
                                      (cadr x)))))
          (lambda (str sc kons pass fail)
            (unpad str
                   sc
                   kons
                   (lambda (str sc fail2)
                     (parse-ref str sc kons pass fail2))
                   fail)))
        unpad)))

(define (temporal-unfit-right n x . o)
  (apply temporal-unpad-right n x o))

(define (temporal-unfit-min-length n x . o)
  n)

(define (temporal-unfit-max-length n x . o)
  n)

(define temporal-unparseable:ref
  (make-unparseable
   (lambda (seq n)
     ((if (vector? seq) vector-ref list-ref) seq n))
   (lambda (seq n)
     (let ((parse (alternate-parser seq)))
       (lambda (str sc kons pass fail)
         (parse str sc
                kons
                (lambda (i str sc fail)
                  ;; convert back to a string to reparse
                  (pass (number->string i) sc fail))
                fail))))))

(define temporal-unparseable:pad
  (make-unparseable temporal-pad
                    temporal-unpad
                    temporal-unpad-min-length
                    temporal-unpad-max-length))

(define temporal-unparseable:pad0
  (make-unparseable (lambda (n x) (temporal-pad n x #\0))
                    temporal-unpad
                    temporal-unpad-min-length
                    temporal-unpad-max-length))

(define temporal-unparseable:pad-right
  (make-unparseable temporal-pad-right
                    temporal-unpad-right
                    temporal-unpad-min-length
                    temporal-unpad-max-length))

(define temporal-unparseable:pad0-right
  (make-unparseable (lambda (n x) (temporal-pad-right n x #\0))
                    temporal-unpad-right
                    temporal-unpad-min-length
                    temporal-unpad-max-length))

(define temporal-unparseable:fit
  (make-unparseable temporal-fit
                    temporal-unfit
                    temporal-unfit-min-length
                    temporal-unfit-max-length))

(define temporal-unparseable:fit0
  (make-unparseable (lambda (n x) (temporal-fit n x #\0))
                    temporal-unfit
                    temporal-unfit-min-length
                    temporal-unfit-max-length))

(define temporal-unparseable:fit-right
  (make-unparseable temporal-fit-right
                    temporal-unfit-right
                    temporal-unfit-min-length
                    temporal-unfit-max-length))

(define temporal-unparseable:fit0-right
  (make-unparseable (lambda (n x) (temporal-fit-right n x #\0))
                    temporal-unfit-right
                    temporal-unfit-min-length
                    temporal-unfit-max-length))

(define simple-trasform-unparseables
  `((,temporal-unparseable:pad
     . ,string-pad)
    (,temporal-unparseable:pad0
     . ,(lambda (s n) (string-pad s n #\0)))
    (,temporal-unparseable:pad-right
     . ,string-pad-right)
    (,temporal-unparseable:pad0-right
     . ,(lambda (s n) (string-pad-right s n #\0)))
    (,temporal-unparseable:fit
     . ,(lambda (s n) (string-pad (string-take s n) n)))
    (,temporal-unparseable:fit0
     . ,(lambda (s n) (string-pad (string-take s n) n #\0)))
    (,temporal-unparseable:fit-right
     . ,(lambda (s n) (string-pad-right (string-take s n) n)))
    (,temporal-unparseable:fit0-right
     . ,(lambda (s n) (string-pad-right (string-take s n) n #\0)))))

(define default-locale (make-locale 'en))

(define (make-locale-lookup chrono names)
  (lambda (locale name)
    (let lp ((messages (list (chronology-messages chrono) names)))
      (and (pair? messages)
           (let lookup ((ls (car messages)) (fallback '()))
             (cond
              ((null? ls)
               (or (any (lambda (x) (cond ((assq name (cdr x)) => cdr) (else #f)))
                        (list-sort (lambda (a b) (locale-includes? b a)) fallback))
                   (lp (cdr messages))))
              ((locale-includes? (caar ls) locale)
               ;; fast path: try an exact match right away
               (if (locale= (caar ls) locale)
                   (cond ((assq name (cdar ls)) => cdr)
                         (else (lookup (cdr ls) fallback)))
                   (lookup (cdr ls) (cons (car ls) fallback))))
              (else
               (lookup (cdr ls) fallback))))))))

;; Analyze, resolving to literal strings, field references, virtual
;; references (as procedures), or unparseable applications (whose
;; arguments may additionally include numbers and vectors of strings).
;; The result can then be compiled either for formatting or parsing.
(define (temporal-analyze fmt chrono . o)
  (let-optionals* o ((locale default-locale)
                     (lookup (make-locale-lookup chrono temporal-names)))
    (let analyze ((x fmt))
      (cond
       ((and (symbol? x) (lookup locale x))
        => (lambda (y)
             (if (or (eq? y x) (and (pair? y) (memq x y)))
                 (error "circular reference in temporal format" fmt x)
                 (analyze y))))
       ((and (symbol? x) (chronology-get-field chrono x)))
       ((and (symbol? x) (assq x (chronology-virtual chrono)))
        => (lambda (cell) (make-virtual-field x (cdr cell))))
       ((chrono-field? x) x)
       ((virtual-field? x) x)
       ((unparseable? x) x)
       ((temporal-macro? x) x)
       ((memq x '(? optional or)) x)
       ((symbol? x)
        (error (string-append "unknown locale formatter for "
                              (symbol->string (chronology-name chrono))
                              " in " (locale->string locale))
               x))
       ((pair? x)
        (let ((op (analyze (car x)))
              (args (map analyze (cdr x))))
          (cond
           ((temporal-macro? op)
            (analyze (apply (temporal-macro-proc op) args)))
           ((and (pair? args) (pair? (cdr args))
                 (pair? (cadr args))
                 (eq? temporal-unparseable:ref (car (cadr args)))
                 (assq op simple-trasform-unparseables))
            ;; special case, precompute simple transforms of ref
            => (lambda (cell)
                 (let ((y `(,temporal-unparseable:ref
                            ,(vector-map (lambda (s) ((cdr cell) s (car args)))
                                          (cadr (cadr args)))
                            ,@(cddr (cadr args)))))
                   (analyze y))))
           (else
            (cons op args)))))
       ((null? x) '())
       ((string? x) x)
       ((number? x) x)
       ((vector? x) x)
       (else (error "unknown locale formatter object" x))))))

(define (chronology-temporal-formatter fmt chrono . o)
  (define (format-to-string f)
    (lambda (t)
      (let ((out (open-output-string)))
        (f t out)
        (get-output-string out))))
  (define (apply-format t op args)
    (apply (unparseable-unparse op)
           (map (lambda (a)
                  (cond
                   ((chrono-field? a)
                    ((chrono-field-getter a) t))
                   ((virtual-field? a)
                    ((virtual-field-proc a) t))
                   ((procedure? a)
                    (a t))
                   ((and (pair? a) (unparseable? (car a)))
                    (apply-format t (car a) (cdr a)))
                   (else
                    a)))
                args)))
  (let-optionals* o ((locale default-locale)
                     (lookup (make-locale-lookup chrono temporal-names)))
    (let compile ((x (temporal-analyze fmt chrono locale lookup))
                  (return format-to-string))
      (cond
       ((chrono-field? x)
        (let ((get (chrono-field-getter x)))
          (return (lambda (t out) (display (get t) out)))))
       ((virtual-field? x)
        (return (lambda (t out) (display ((virtual-field-proc x) t) out))))
       ((and (pair? x) (unparseable? (car x)))
        (return (lambda (t out)
                  (display (apply-format t (car x) (cdr x))
                   out))))
       ((and (pair? x) (null? (cdr x)))
        (compile (car x) return))
       ((pair? x)
        (if (memq (car x) '(? optional or))
            (compile (cadr x) return)
            (let ((ls (map (lambda (a) (compile a (lambda (f) f))) x)))
              (return (lambda (t out)
                        (for-each (lambda (f) (f t out)) ls))))))
       ((null? x)
        (return (lambda (t out) '())))
       (else
        (return (lambda (t out) (display x out))))))))

(define (temporal->string t . o)
  (let ((chronology (temporal-chronology t)))
    (let-optionals o ((fmt (chronology-format chronology))
                      (locale default-locale))
      ((chronology-temporal-formatter fmt chronology locale)
       t))))

(define (parse-integer str sc fixed-len pass fail)
  (let lp ((sc sc)
           (i 0)
           (res 0)
           (fail fail))
    (cond
     ((and fixed-len (= i fixed-len))
      (pass res str sc fail))
     ((and (string-cursor<? sc (string-cursor-end str))
           (char-numeric? (string-ref/cursor str sc)))
      (lp (string-cursor-next str sc)
          (+ i 1)
          (+ (* res 10) (digit-value (string-ref/cursor str sc)))
          ;; TODO: determine when we can disable backtracking
          (if (and (positive? i) (not fixed-len))
              (lambda (msg str2)
                ;; use the earliest failure message when backtracking
                (let ((fail2 (lambda (msg2 str3) (fail msg str2))))
                  (pass res str sc fail2)))
              fail)))
     ((or (zero? i) fixed-len)
      (fail (string-append "insufficient digits at: "
                           (number->string (string-cursor->index str sc)))
            str))
     (else
      (pass res str sc fail)))))

(define (string-is-prefix-at prefix str sc)
  (let lp ((scp (string-cursor-start prefix))
           (scs sc))
    (cond
     ((string-cursor>=? scp (string-cursor-end prefix))
      scs)
     ((string-cursor>=? scs (string-cursor-end str))
      #f)
     ((char=? (string-ref/cursor prefix scp) (string-ref/cursor str scs))
      (lp (string-cursor-next prefix scp) (string-cursor-next str scs)))
     (else
      #f))))

(define (parse-literal str sc expect pass fail)
  (cond
   ((string-is-prefix-at expect str sc)
    => (lambda (sc) (pass sc)))
   (else
    (fail sc))))

(define (parse-pred str start pred proc)
  (let lp ((sc start))
    (if (or (string-cursor>=? sc (string-cursor-end str))
            (not (pred (string-ref/cursor str sc))))
        (proc (substring/cursors str start sc) sc)
        (lp (string-cursor-next str sc)))))

(define (alternate-parser vec)
  ;; TODO: consider precompiling a state machine
  (lambda (str sc kons pass fail)
    (let lp ((i 0))
      (cond
       ((>= i (vector-length vec))
        (fail (string-append "no known literal matched: " (->string vec)) str))
       ((string? (vector-ref vec i))
        (parse-literal str sc (vector-ref vec i)
                       (lambda (sc) (pass i str sc fail))
                       (lambda (sc) (lp (+ i 1)))))
       (else
        (lp (+ i 1)))))))

(define (chrono-field-parser field)
  (let ((name (chrono-field-name field)))
    ;; TODO: parse non-integer fields
    (let ((lb (and (not (chrono-field-get-lb field)) (chrono-field-lb field)))
          (ub (and (not (chrono-field-get-ub field)) (chrono-field-ub field))))
      (if (or lb ub)
          (lambda (ls str sc kons pass fail)
            (parse-integer
             str sc #f
             (lambda (i str sc fail)
               ;; TODO: handle dynamic bounds
               (if (and (or (not lb) (<= lb i)) (or (not ub) (<= i ub)))
                   (pass (kons name i ls) str sc fail)
                   (fail (string-append "out of bounds for " (->string name)
                                        ": " (number->string i))
                         str)))
             fail))
          (lambda (ls str sc kons pass fail)
            (parse-integer
             str sc #f
             (lambda (i str sc fail)
               (pass (kons name i ls) str sc fail))
             fail))))))

(define (virtual-field-parser virtual-field)
  (let ((name (virtual-field-name virtual-field)))
    (lambda (ls str sc kons pass fail)
      (parse-integer
       str sc #f
       (lambda (i str sc fail)
         (pass (cons (cons name i) ls) str sc fail))
       fail))))

(define (temporal-offset->string t)
  (string-join (map ->string (temporal-offset t)) ":"))

(define (temporal-time-zone-abbrev t)
  (let* ((info (time-zone-info (temporal-ref t 'time-zone) t))
         (format (tz-info-format info)))
    ;; TODO: DST formatting
    format))

(define temporal-names
  `((,(make-locale 'en)
     (day-of-week-names
      . #("Sunday" "Monday" "Tuesday" "Wednesday"
          "Thursday" "Friday" "Saturday"))
     (month-names
      . #("<invalid>" "January" "February" "March" "April" "May" "June"
          "July" "August" "September" "October" "November" "December"))
     (nth-names . #("th" "st" "nd" "rd"))
     (am/pm-names . #("AM" "PM")))
    (,(make-locale 'fr)
     (day-of-week-names
      . #("dimanche" "lundi" "mardi" "mercredi" "jeudi" "vendredi" "samedi"))
     (month-names
      . #("<invalid>" "janvier" "février" "mars" "avril" "mai" "juin"
          "juillet" "août" "septembre" "octobre" "novembre" "décembre"))
     (nth-names . #("ème" "er")))
    (,(make-locale 'ja)
     (day-of-week-names
      . #("日曜日" "月曜日" "火曜日" "水曜日" "木曜日" "金曜日" "土曜日"))
     (month-names
      . #("<invalid>" "一月" "二月" "三月" "四月" "五月" "六月"
          "七月" "八月" "九月" "十月" "十一月" "十二月"))
     (nth-names . #("")))
    (,(make-locale #f)
     (day-of-week-name . (ref day-of-week-names day-of-week))
     (month-name . (ref month-names month))
     (ordinal
      . ,(make-unparseable
          (lambda (n) (case (modulo n 10) ((1 2 3) n) (else 0)))))
     (nth . ,(make-temporal-macro (lambda (x) `(ref nth-names (ordinal ,x)))))
     (ref . ,temporal-unparseable:ref)
     ;; TODO: link
     (quotient
      . ,(make-unparseable
          (lambda (x m)
            (quotient x m))
          (lambda (x m)
            (lambda (str sc kons pass fail)
              (parse-integer
               str sc #f
               (lambda (i str sc fail)
                 (pass (number->string i) sc fail))
               fail)))
          #f
          #f
          (lambda (x m)
            (lambda (name value ls)
              (cons (cons name (list 'solve
                                     (lambda (x) (+ (* value m) x))
                                     (lambda (v) (= value (quotient v m)))))
                    ls)))))
     (modulo
      . ,(make-unparseable
          (lambda (x m)
            (modulo x m))
          (lambda (x m)
            (lambda (str sc kons pass fail)
              (parse-integer
               str sc #f
               (lambda (i str sc fail)
                 (pass (number->string i) sc fail))
               fail)))
          #f
          #f
          (lambda (x m)
            (lambda (name value ls)
              (cons (cons name (list 'solve
                                     (lambda (x) (+ value (* x m)))
                                     (lambda (v) (= value (modulo v m)))))
                    ls)))))
     (am/pm
      (ref am/pm-names (quotient hour 12)))
     (time-zone-abbrev
      . ,(make-unparseable
          temporal-time-zone-abbrev
          (lambda _
            (lambda (str sc kons pass fail)
              (parse-pred str sc char-alphabetic?
                          (lambda (tz-str sc2)
                            (cond
                             ((string->time-zone tz-str)
                              => (lambda (tz) (pass tz sc2 fail)))
                             (else (fail "unknown timezone" str)))))))))
     (time-zone-offset
      . ,(make-unparseable
          (lambda (x) (temporal-offset->string (temporal-ref x 'time-zone)))
          string->time-zone
          (lambda _
            (lambda (str sc kons pass fail)
              (parse-pred str sc (lambda (ch)
                                   (or (char-numeric? ch)
                                       (memv ch '(#\+ #\- #\:))))
                          (lambda (tz-str sc2)
                            (cond
                             ((string->time-zone tz-str)
                              => (lambda (tz) (pass tz sc2 fail)))
                             (else (fail "unknown timezone" str)))))))))
     (pad . ,temporal-unparseable:pad)
     (pad0 . ,temporal-unparseable:pad0)
     (pad-right . ,temporal-unparseable:pad-right)
     (pad0-right . ,temporal-unparseable:pad0-right)
     (fit . ,temporal-unparseable:fit)
     (fit0 . ,temporal-unparseable:fit0)
     (fit-right . ,temporal-unparseable:fit-right)
     (fit0-right . ,temporal-unparseable:fit0-right)
     )))

(define (chronology-temporal-parser fmt chrono . o)
  (let-optionals* o ((locale default-locale)
                     (strict? #f)
                     (lookup (make-locale-lookup chrono temporal-names)))
    ;; A compiled parser is a CPS-style procedure of the form:
    ;;   (parse ls str sc kons pass fail)
    ;; where ls is the accumulated alist of temporal key/values, str
    ;; is the input string, sc the current cursor, kons the
    ;; accumulator for new fields, and pass and fail the continuation
    ;; thunks.  kons takes the signature:
    ;;   (kons field-name field-value ls)
    ;; while pass takes the signature:
    ;;   (pass ls str sc fail)
    ;; and fail is of the form
    ;;   (fail reason str)
    ;; In addition, we track optional validators for virtual fields,
    ;; such as day of week, and the compile step returns both.
    (let compile ((x (temporal-analyze fmt chrono locale lookup))
                  (return
                   ;; The final return wraps the parser combinators in
                   ;; the initial procedure of one string argument
                   ;; which kicks off the parsing, converts the result
                   ;; to a temporal, and validates it.
                   (lambda (f)
                     (let ((pass
                            (lambda (ls str sc fail)
                              (if (string-cursor>=? sc (string-cursor-end str))
                                  ls
                                  (fail "trailing data in string" str))))
                           (fail
                            (lambda (msg str)
                              (error "couldn't parse temporal" msg str))))
                       (lambda (str)
                         (chronology-try-alist->temporal
                          (f '() str (string-cursor-start str)
                             (lambda (name value ls)
                               (cons (cons name value) ls))
                             pass fail)
                          values
                          (lambda (res err)
                            (if strict?
                                (fail (apply string-append (map ->string err))
                                      str)
                                res))
                          chrono))))))
      (cond
       ;; ((and (pair? x) (pair? (cdr x)) (pair? (car x)) (pair? (cadr x))
       ;;       (unparseable? (caar x)) (unparseable? (car (cadr x)))
       ;;       (not (unparseable-fixed-length? (caar x) (cdar x)))
       ;;       (unparseable-fixed-length? (car (cadr x)) (cdr (cadr x))))
       ;;  ;; try to avoid backtracking
       ;;  )
       ((and (pair? x) (unparseable? (car x)))
        (assert (procedure? (unparseable-parse (car x))))
        ;; An unparseable knows how to parse the string, but may need
        ;; to pass that value to the underlying parser.  It should
        ;; wrap at most one field, and assume the other arguments are
        ;; passed directly to the parser.  Note parsing happens
        ;; "inside-out" of the traditional sense, in that we apply the
        ;; outer parser before passing that result to the inner.
        (let ((fields (filter (lambda (x)
                                (or (chrono-field? x)
                                    (and (pair? x)
                                         (unparseable? (car x))
                                         (unparseable-parse (car x)))))
                              (cdr x))))
          (case (length fields)
            ((0)
             (let ((parse (apply (unparseable-parse (car x)) (cdr x))))
               (return (lambda (ls str sc kons pass fail)
                         ;; just drop the parsed value
                         (parse str sc kons
                                (lambda (substr sc fail)
                                  (pass ls str sc fail))
                                fail)))))
            ((1)
             (let ((parse-field (compile (car fields) values))
                   (parse (apply (unparseable-parse (car x)) (cdr x))))
               (return (lambda (ls str sc kons pass fail)
                         (parse str sc
                                kons
                                (lambda (substr sc2 fail)
                                  (parse-field ls
                                               substr
                                               (string-cursor-start substr)
                                               (cond
                                                ((unparseable-kons (car x))
                                                 => (lambda (make-kons)
                                                      (apply make-kons
                                                             (cdr x))))
                                                (else kons))
                                               (lambda (ls substr subsc fail)
                                                 (pass ls str sc2 fail))
                                               fail))
                                fail)))))
            (else
             (error "can't parse more than one field with unparseable" x)))))
       ((pair? x)
        (cond
         ;; TODO: Generalize unparseables to support optional/or.
         ((memq (car x) '(? optional))
          (assert (= 2 (length x)))
          (let ((parse-body (compile (second x) values)))
            (return
             (lambda (ls str sc kons pass fail)
               ;; Override fail to just call pass on the current state.
               (parse-body ls str sc kons pass
                           (lambda (reason err-str)
                             (pass ls str sc fail)))))))
         ((eq? (car x) 'or)
          (case (length x)
            ((1) (return values))
            ((2) (compile (second x) return))
            (else
             (let ((parse-car (compile (second x) values))
                   (parse-cdr (if (= 3 (length x))
                                  (compile (third x) values)
                                  (compile `(or ,(cddr x)) values))))
               (return
                (lambda (ls str sc kons pass fail)
                  ;; Override fail to fall back to alternatives.
                  (parse-car ls str sc kons pass
                             (lambda (reason err-str)
                               (parse-cdr ls str sc fail)))))))))
         (else
          ;; Otherwise assume a pair is an implicit sequence.
          (let ((parse-car (compile (car x) values))
                (parse-cdr (compile (cdr x) values)))
            (return (lambda (ls str sc kons pass fail)
                      (parse-car ls str sc
                                 kons
                                 (lambda (ls str sc fail)
                                   (parse-cdr ls str sc kons pass fail))
                                 fail)))))))
       ((null? x)
        (return (lambda (ls str sc kons pass fail) (pass ls str sc fail))))
       ((chrono-field? x)
        (return (chrono-field-parser x)))
       ((virtual-field? x)
        (return (virtual-field-parser x)))
       ((string? x)
        (return
         (lambda (ls str sc kons pass fail)
           (parse-literal
            str sc x
            (lambda (sc) (pass ls str sc fail))
            (lambda (sc)
              (fail (string-append
                     "expected value not found at "
                     (number->string (string-cursor->index str sc))
                     ": '" x "'")
                    str))))))
       ((number? x)
        ;; TODO: allow alternate numeric formats
        (compile (number->string x) return))
       (else
        (error "unknown locale parser object" locale x))))))

(define (chronology-string->temporal str fmt chrono . o)
  ((apply chronology-temporal-parser fmt chrono o) str))
