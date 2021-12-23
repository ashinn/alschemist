
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
;; (fix[0][-right] n x [ch]): pads or truncates as needed
;; (ordinal x): locale-specific ordinal enum (en = st: 0, nd: 1, rd: 2, th: 3)
;; (ref #("st" "nd" "rd" "th") (ordinal x))
;; (nth x): short for (ref nth-names (ordinal x))

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

(define (temporal-fix n x . o)
  (string-truncate n (apply temporal-pad n x o)))

(define (temporal-fix-right n x . o)
  (string-truncate-right n (apply temporal-pad-right n x o)))

(define (temporal-unpad n x . o)
  (lambda (str sc pass fail)
    (let ((sc2 (string-cursor-forward str sc n)))
      (if (and (string-cursor<? sc sc2)
               (string-cursor<=? sc2 (string-cursor-end str)))
          (pass (substring/cursors str sc sc2) sc2 fail)
          (fail "no input to unpad" str)))))

(define temporal-unfix temporal-unpad)

(define (temporal-unpad-min-length n x . o)
  n)

(define (temporal-unpad-max-length n x . o)
  +inf.0)

(define (temporal-unfix-min-length n x . o)
  n)

(define (temporal-unfix-max-length n x . o)
  n)

(define-record-type Temporal-Macro
  (make-temporal-macro proc)
  temporal-macro?
  (proc temporal-macro-proc))

;; invertible, returning first argument
(define-record-type Unparseable
  (%make-unparseable unparse parse min-length max-length)
  unparseable?
  (unparse unparseable-unparse)
  (parse unparseable-parse)
  (min-length unparseable-min-length)
  (max-length unparseable-max-length))

(define (make-unparseable unparse parse . o)
  (let-optionals o ((min-length (lambda (ls) 0))
                    (max-length (lambda (ls) +inf.0)))
    (%make-unparseable unparse parse min-length max-length)))

(define (unparseable-fixed-length? p args)
  (and (unparseable-min-length p)
       (unparseable-max-length p)
       (= (apply (unparseable-min-length p) args)
          (apply (unparseable-max-length p) args))))

(define default-locale (make-locale 'en))

(define (make-locale-lookup names)
  (lambda (locale name)
    (let lookup ((ls names) (fallback '()))
      (cond
       ((null? ls)
        (any (lambda (x) (cond ((assq name (cdr x)) => cdr) (else #f)))
             (list-sort (lambda (a b) (locale-includes? b a)) fallback)))
       ((locale-includes? (caar ls) locale)
        ;; fast path: try an exact match right away
        (if (locale= (caar ls) locale)
            (cond ((assq name (cdar ls)) => cdr)
                  (else (lookup (cdr ls) fallback)))
            (lookup (cdr ls) (cons (car ls) fallback))))
       (else
        (lookup (cdr ls) fallback))))))

;; Analyze, resolving to literal strings, field references, virtual
;; references (as procedures), or unparseable applications (whose
;; arguments may additionally include numbers and vectors of strings).
;; The result can then be compiled either for formatting or parsing.
(define (temporal-analyze fmt . o)
  (let-optionals* o ((chrono (default-chronology))
                     (locale default-locale)
                     (lookup (make-locale-lookup temporal-names)))
    (let analyze ((x fmt))
      (cond
       ((and (symbol? x) (lookup locale x))
        => (lambda (y)
             (if (or (eq? y x) (and (pair? y) (memq x y)))
                 (error "circular reference in temporal format" fmt x)
                 (analyze y))))
       ((and (symbol? x) (chronology-get-field chrono x)))
       ((and (symbol? x) (assq x (chronology-virtual chrono)))
        => (lambda (cell) (cdr cell)))
       ((chrono-field? x) x)
       ((unparseable? x) x)
       ((temporal-macro? x) x)
       ((symbol? x)
        (error (string-append "unknown locale formatter for "
                              (symbol->string (chronology-name chrono))
                              " in " (locale->string locale))
               x))
       ((pair? x)
        (let ((op (analyze (car x)))
              (args (analyze (cdr x))))
          (if (temporal-macro? op)
              (analyze (apply (temporal-macro-proc op) args))
              (cons op args))))
       ((null? x) '())
       ((string? x) x)
       ((number? x) x)
       ((vector? x) x)
       (else (error "unknown locale formatter object" x))))))

(define (temporal-formatter fmt . o)
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
                   ((procedure? a)
                    (a t))
                   ((and (pair? a) (unparseable? (car a)))
                    (apply-format t (car a) (cdr a)))
                   (else
                    a)))
                args)))
  (let-optionals* o ((chrono (default-chronology))
                     (locale default-locale)
                     (lookup (make-locale-lookup temporal-names)))
    (let compile ((x (temporal-analyze fmt chrono locale lookup))
                  (return format-to-string))
      (cond
       ((chrono-field? x)
        (let ((get (chrono-field-getter x)))
          (return (lambda (t out) (display (get t) out)))))
       ((procedure? x)
        (return (lambda (t out) (display ((x t) out)))))
       ((and (pair? x) (unparseable? (car x)))
        (return (lambda (t out)
                  (display (apply-format t (car x) (cdr x))
                   out))))
       ((and (pair? x) (null? (cdr x)))
        (compile (car x) return))
       ((pair? x)
        (let ((ls (map (lambda (a) (compile a (lambda (f) f))) x)))
          (return (lambda (t out)
                    (for-each (lambda (f) (f t out)) ls)))))
       ((null? x)
        (return (lambda (t out) '())))
       (else
        (return (lambda (t out) (display x out))))))))

(define (temporal->string t fmt . o)
  ((temporal-formatter fmt
                       (temporal-chronology t)
                       (if (pair? o) (car o) default-locale))
   t ))

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
                (pass res str sc fail))
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

(define (alternate-parser vec)
  ;; TODO: consider precompiling a state machine
  (lambda (str sc pass fail)
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
    (if (or (chrono-field-lb field) (chrono-field-ub field))
        (let ((lb (chrono-field-lb field))
              (ub  (chrono-field-ub field)))
          (lambda (ls str sc pass fail)
            (parse-integer
             str sc #f
             (lambda (i str sc fail)
               ;; TODO: handle dynamic bounds
               (if (and (or (not lb) (<= lb i)) (or (not ub) (<= i ub)))
                   (pass (cons (cons name i) ls) str sc fail)
                   (fail (string-append "out of bounds for " (->string name)
                                        ": " (number->string i))
                         str)))
             fail)))
        (lambda (ls str sc pass fail)
          (parse-integer
           str sc #f
           (lambda (i str sc fail)
             (pass (cons (cons name i) ls) str sc fail))
           fail)))))

(define temporal-names
  `((,(make-locale 'en)
     (day-of-week-names
      . #("Sunday" "Monday" "Tuesday" "Wednesday"
          "Thursday" "Friday" "Saturday"))
     (month-names
      . #("<invalid>" "January" "February" "March" "April" "May" "June"
          "July" "August" "September" "October" "November" "December"))
     (nth-names . #("th" "st" "nd" "rd")))
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
          (lambda (n)
            (case (modulo n 10) ((1 2 3) n) (else 0)))
          (lambda (n)
            (lambda (str sc pass fail)
              (error "unimplemented")))))
     (nth . ,(make-temporal-macro (lambda (x) `(ref nth-names (ordinal ,x)))))
     (ref
      . ,(make-unparseable
          (lambda (seq n) ((if (vector? seq) vector-ref list-ref) seq n))
          (lambda (seq n)
            (let ((parse (alternate-parser seq)))
              (lambda (str sc pass fail)
                (parse str sc
                       (lambda (i str sc fail)
                         ;; convert back to a string to reparse
                         (pass (number->string i) sc fail))
                       fail))))))
     (pad
      . ,(make-unparseable temporal-pad
                           temporal-unpad
                           temporal-unpad-min-length
                           temporal-unpad-max-length))
     (pad0
      . ,(make-unparseable (lambda (n x) (temporal-pad n x #\0))
                           temporal-unpad
                           temporal-unpad-min-length
                           temporal-unpad-max-length))
     (pad-right
      . ,(make-unparseable temporal-pad-right
                           temporal-unpad
                           temporal-unpad-min-length
                           temporal-unpad-max-length))
     (pad0-right
      . ,(make-unparseable (lambda (n x) (temporal-pad-right n x #\0))
                           temporal-unpad
                           temporal-unpad-min-length
                           temporal-unpad-max-length))
     (fix
      . ,(make-unparseable temporal-fix
                           temporal-unfix
                           temporal-unfix-min-length
                           temporal-unfix-max-length))
     (fix0
      . ,(make-unparseable (lambda (n x) (temporal-fix n x #\0))
                           temporal-unfix
                           temporal-unfix-min-length
                           temporal-unfix-max-length))
     (fix-right
      . ,(make-unparseable temporal-fix-right
                           temporal-unfix
                           temporal-unfix-min-length
                           temporal-unfix-max-length))
     (fix0-right
      . ,(make-unparseable (lambda (n x) (temporal-fix-right n x #\0))
                           temporal-unfix
                           temporal-unfix-min-length
                           temporal-unfix-max-length))
     )))

(define (temporal-parser fmt . o)
  (let-optionals* o ((chrono (default-chronology))
                     (locale default-locale)
                     (lookup (make-locale-lookup temporal-names))
                     (strict? #f))
    ;; A compiled parser is a CPS-style procedure of the form:
    ;;   (parse ls str sc pass fail)
    ;; where ls is the accumulated alist of temporal key/values,
    ;; str is the input string, sc the current cursor, and pass and
    ;; fail the continuation thunks.  pass takes the signature:
    ;;   (pass ls str sc fail)
    ;; whereas fail is of the form
    ;;   (fail reason str)
    ;; In addition, we track optional validators for virtual fields,
    ;; such as day of week, and the compile step returns both.
    (let compile ((x (temporal-analyze fmt chrono locale lookup))
                  (validate '())
                  (return
                   ;; The final return wraps the parser combinators in
                   ;; the initial procedure of one string argument
                   ;; which kicks off the parsing, converts the result
                   ;; to a temporal, and validates it.
                   (lambda (f validate)
                     (let ((pass (lambda (ls str sc fail) ls))
                           (fail (lambda (msg str)
                                   (error "couldn't parse temporal" msg str))))
                       (if (and strict? (pair? validate))
                          (lambda (str)
                            (let* ((sc (string-cursor-start str))
                                   (res
                                    (alist->temporal
                                     (f '() str sc pass fail)
                                     chrono)))
                              (unless (every validate res)
                                (error "validation failed" res validate))
                              res))
                          (lambda (str)
                            (alist->temporal
                             (f '() str (string-cursor-start str) pass fail)
                             chrono)))))))
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
        ;; passed directly to the parser.
        (let ((fields (filter chrono-field? (cdr x))))
          (case (length fields)
            ((0)
             (let ((parse (apply (unparseable-parse (car x)) (cdr x))))
               (return (lambda (ls str sc pass fail)
                         ;; just drop the parsed value
                         (parse str sc
                                (lambda (substr sc fail)
                                  (pass ls str sc fail))
                                fail))
                      validate)))
            ((1)
             (let-values (((parse-field _) (compile (car fields) '() values))
                          ((parse) (apply (unparseable-parse (car x)) (cdr x))))
               (return (lambda (ls str sc pass fail)
                         (parse str sc
                                (lambda (substr sc fail)
                                  (parse-field ls
                                               substr
                                               (string-cursor-start substr)
                                               (lambda (ls substr subsc fail)
                                                 (pass ls str sc fail))
                                               fail))
                                fail))
                       validate)))
            (else
             (error "can't parse more than one field with unparseable" x)))))
       ((pair? x)
        ;; Otherwise assume a pair is an implicit sequence.
        (let*-values
            (((parse-car validate2) (compile (car x) validate values))
             ((parse-cdr validate3) (compile (cdr x) validate2 values)))
          (return (lambda (ls str sc pass fail)
                    (parse-car ls str sc
                               (lambda (ls str sc fail)
                                 (parse-cdr ls str sc pass fail))
                               fail))
                  validate3)))
       ((null? x)
        (return (lambda (ls str sc pass fail) (pass ls str sc fail))
                validate))
       ((procedure? x)
        (return (lambda (ls str sc pass fail) (pass ls str sc fail))
                (cons x validate)))
       ((chrono-field? x)
        (return (chrono-field-parser x) validate))
       ((string? x)
        (return
         (lambda (ls str sc pass fail)
           (parse-literal
            str sc x
            (lambda (sc) (pass ls str sc fail))
            (lambda (sc)
              (fail (string-append
                     "expected value not found at "
                     (number->string (string-cursor->index str sc)) ": " x)
                    str))))
         validate))
       ((number? x)
        ;; TODO: allow alternate numeric formats
        (compile (number->string x) validate return))
       (else
        (error "unknown locale parser object" locale x))))))

(define (string->temporal str fmt . o)
  ((apply temporal-parser fmt o) str))
