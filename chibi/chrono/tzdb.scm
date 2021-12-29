
;;> Library for parsing tzdb formats.

(define (strip-comment str)
  (cond
   ((string-index str #\#)
    => (lambda (sc)
         (substring/cursors str (string-cursor-start str) sc)))
   (else str)))

(define (split-space str)
  (let ((end (string-cursor-end str)))
    (let lp ((from (string-cursor-start str))
             (to  (string-cursor-start str))
             (ls '()))
      (cond
       ((string-cursor>=? to end)
        (reverse (cons (substring/cursors str from to) ls)))
       ((char-whitespace? (string-ref/cursor str to))
        (let ((next (string-cursor-next str to)))
          (lp next next (cons (substring/cursors str from to) ls))))
       (else
        (lp from (string-cursor-next str to) ls))))))

(define (parse-time str)
  (map string->number (string-split str ":")))

(define (parse-dow str)
  (and (not (equal? "" str))
       (case (string-ref str 0)
         ((#\s #\S)
          (case (and (> (string-length str) 1) (string-ref str 1))
            ((#\u #\U) 0)
            (else 6)))
         ((#\m #\M) 1)
         ((#\t #\T)
          (case (and (> (string-length str) 1) (string-ref str 1))
            ((#\u #\U) 2)
            (else 4)))
         ((#\w #\W) 3)
         ((#\f #\F) 5)
         (else (string->number str)))))

(define (parse-month str)
  (and (not (equal? "" str))
       (case (string-ref str 0)
         ((#\j #\J)
          (case (and (> (string-length str) 1) (string-ref str 1))
            ((#\a #\A) 1)
            (else
             (case (and (> (string-length str) 2) (string-ref str 2))
               ((#\n #\N) 6)
               (else 7)))))
         ((#\m #\M)
          (case (and (> (string-length str) 2) (string-ref str 2))
            ((#\y #\Y) 5)
            (else 3)))
         ((#\a #\A)
          (case (and (> (string-length str) 1) (string-ref str 1))
            ((#\p #\P) 4)
            (else 8)))
         ((#\f #\F) 2)
         ((#\s #\S) 9)
         ((#\o #\O) 10)
         ((#\n #\N) 11)
         ((#\d #\D) 12)
         (else (string->number str)))))

(define (parse-day-of-month str)
  (cond
   ((string->number str))
   ((string-prefix? "last" str)
    ;; TODO: except for Feb this can be converted to the >= form
    (list 'last (parse-dow (string-drop str 4))))
   ((string-contains str "<=")
    => (lambda (sc)
         (list '<=
               (parse-dow
                (substring/cursors str (string-cursor-start str) sc))
               (string->number
                (substring/cursors str
                                   (string-cursor-forward str sc 2)
                                   (string-cursor-end str))))))
   ((string-contains str ">=")
    => (lambda (sc)
         (list '>=
               (parse-dow
                (substring/cursors str (string-cursor-start str) sc))
               (string->number
                (substring/cursors str
                                   (string-cursor-forward str sc 2)
                                   (string-cursor-end str))))))
   (else
    (error "unknown day of month in rule" str))))

(define (parse-rule fields)
  (list (string->symbol (first fields))
        (list 
         (string->number (second fields))
         (case (string->symbol (third fields))
           ((o on onl only O ONLY) (string->number (second fields)))
           ((mi min MI MIN) -inf.0)
           ((ma max MA MAX) +inf.0)
           (else (string->number (third fields))))
         (or (parse-month (fifth fields))
             (error "not a month" (fifth fields) fields))
         (parse-day-of-month (sixth fields))
         (parse-time (string-trim-right (seventh fields) char-alphabetic?))
         (parse-time (string-trim-right (eighth fields) char-alphabetic?))
         (ninth fields))))

(define (parse-zone fields in)
  (define (resolve fields)
    `(,(parse-time (first fields))
      ,(string->symbol (second fields))
      ,(third fields)
      ,@(if (> (length fields) 3)
            `((,(string->number (fourth fields))
               ,@(if (> (length fields) 4)
                     (list (parse-month (fifth fields)))
                     '())
               ,@(if (> (length fields) 5)
                     (list (string->number (sixth fields)))
                     '())
               ,@(if (> (length fields) 6)
                     (list (parse-time (seventh fields)))
                     '())))
            '())))
  (let ((name (car fields)))
    (let lp ((ls (list (resolve (cdr fields)))))
      (let ((ch (peek-char in)))
        (cond
         ((and (char? ch) (or (char-numeric? ch) (eqv? ch #\-)))
          (let ((line (split-space
                       (string-trim (strip-comment (read-line in))))))
            (lp (cons (resolve line) ls))))
         (else
          (cons name (reverse ls))))))))

;; return only currenetly active rules
(define (compress-zone zone)
  (cons (car zone)
        (filter (lambda (x) (= 3 (length x))) (cdr zone))))
(define (compress-rule rule)
  (cons (car rule)
        (filter (lambda (x) (= (cadr x) +inf.0)) (cdr rule))))

;;> Parses the given input port in zic text format and returns an alist
;;> of the zones, rules and links in sexp format:
;;>   zones: (name (stdoff rules format [until]) ...)
;;>   rules: (name (from-year to-year month (day dow nth) time save letter) ...)
;;>   links: (alias . target)
;;> If the optional \var{compress?} is given and true, strips
;;> historical information returning only currently active rules.
(define (tzdb-parse in . o)
  (let ((compress? (and (pair? o) (car o))))
    (let lp ((zones '()) (rules '()) (links '()))
      (let ((line (read-line in)))
        (cond
         ((eof-object? line)
          `((zones ,@(reverse (if compress? (map compress-zone zones) zones)))
            (rules ,@(reverse
                      (remove
                       (lambda (x) (null? (cdr x)))
                       (map (lambda (x)
                              ((if compress? compress-rule values)
                               (cons (car x) (reverse (cdr x)))))
                            rules))))
            (links ,@(reverse links))))
         (else
          ;; TODO: allow quoted special chars, though they seem unused
          ;; in practice
          (let ((clean-line (string-trim (strip-comment line))))
            (if (equal? clean-line "")
                (lp zones rules links)
                (let ((fields (split-space clean-line)))
                  (case (string->symbol (car fields))
                    ((R RULE Rule r ru rul rule)
                     (let ((rule (parse-rule (cdr fields))))
                       (cond
                        ((and (pair? rules) (eq? (car rule) (caar rules)))
                         (lp zones
                             (cons (append rule (cdar rules)) (cdr rules))
                             links))
                        (else (lp zones (cons rule rules) links)))))
                    ((Z ZONE Zone z zo zon zone)
                     (lp (cons (parse-zone (cdr fields) in) zones) rules links))
                    ((L LINK Link l li lin link)
                     (lp zones
                         rules
                         (cons (cons (car (cddr fields)) (cadr fields)) links)))
                    (else
                     (error "unknown line in tzdb" clean-line (car fields)))
                    ))))))))))

;;> Parses the given file in zic text format with \scheme{tzdb-parse}.
(define (tzdb-parse-file path)
  (call-with-input-file path
    tzdb-parse))

;;> Parses the given input port in zone file format and returns a list
;;> of zone vectors, with fields:
;;>   \scheme{#(zone-name countries lat+lng comment)}
(define (tzdb-parse-zones in)
  (let lp ((res '()))
    (let ((line (read-line in)))
      (cond
       ((eof-object? line)
        (reverse res))
       ((or (equal? line "") (eqv? #\# (string-ref line 0)))
        (lp res))
       (else
        (let ((fields (split-space line)))
          (cond
           ((<= 3 (length fields) 4)
            (let ((zone (list-ref fields 2))
                  (countries (map string->symbol
                                  (string-split (car fields) ",")))
                  (lat+lng (list-ref fields 1))
                  (comment (and (= (length fields) 4) (list-ref fields 3))))
              (lp (cons (vector zone countries lat+lng comment) res))))
           (else
            (write-string ";; invalid zone row: " (current-error-port))
            (write-string line (current-error-port))
            (newline (current-error-port))
            (lp res)))))))))

;;> Parses the given zone file with \scheme{tzdb-parse-zones}.
(define (tzdb-parse-zone-file path)
  (call-with-input-file path
    tzdb-parse-zones))
