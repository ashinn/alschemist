
(define (string-translate str ch1 ch2)
  (string-map (lambda (ch) (if (eqv? ch ch1) ch2 ch)) str))

;; translate a wiki word to a safe pathname (after escaping, colons
;; ':' are translated to slashes '/' to create a diretory hierarchy of
;; wiki words)
(define wiki-word-encode
  (let ((wiki-unsafe-rx (regexp '(~ (/ "!-") (/ "0Z") (/ "^~")))))
    (lambda (str)
      (string-translate
       (regexp-replace-all
        wiki-unsafe-rx
        str
        (lambda (m)
          (let ((n (char->integer (string-ref (regexp-match-submatch m 0) 0))))
            (string-append "_" (if (< n 16) "0" "")
                           (string-upcase-ascii (number->string n 16))))))
       #\: #\/))))

;; reverse the above
(define wiki-word-decode
  (let ((wiki-escaped-rx (regexp '(: "_(" (= 2 (/ "09" "af" "AF")) ")"))))
    (lambda (str)
      (regexp-replace-all
       wiki-escaped-rx
       (string-translate str #\/ #\:)
       (lambda (m)
         (string
          (integer->char
           (string->number (regexp-match-submatch m 1) 16))))))))

(define (%regexp-multi-fold ls str start end)
  (cond
   ((null? ls)
    (if (>= start end) '() (list (substring str start end))))
   (else
    (regexp-fold
     (caar ls)
     (lambda (i m str x)
       (let ((left (%regexp-multi-fold
                    (cdr ls) str i (regexp-match-submatch-start m 0)))
             (right (list ((cadr (car ls)) m))))
         (append x left right)))
     '()
     str
     (lambda (i m str x)
       (append x (%regexp-multi-fold (cdr ls) str i end)))
     start
     end))))

(define (regexp-multi-fold ls str . o)
  (let ((start (if (pair? o) (car o) 0))
        (end (if (and (pair? o) (pair? (cdr o)))
                 (cadr o)
                 (string-length str))))
    (%regexp-multi-fold ls str start end)))

(define wiki-parse-inline
  (let ((wiki-bold-rx
         (regexp '(: "'''" ($ (* (~ ("'")))) "'''")))
        (wiki-italic-rx
         (regexp '(: "''" ($ (* (~ ("'")))) "''")))
        (wiki-uline-rx
         (regexp '(: "<u>" ($ (* (~ ("<")))) "</u>")))
        (wiki-strike-out-rx
         (regexp '(: "<s>" ($ (* (~ ("<")))) "</s>")))
        (wiki-table-rx
         (regexp '(: "{|" ($ (* nonl)) "\n"
                     ($ (* (or (: "|" (~ ("}"))) (~ ("|")))))
                     "|}")))
        (wiki-row-rx
         (regexp '(: "\n|-" (* space))))
        (wiki-col-rx
         (regexp "||"))
        (wiki-note-rx
         (regexp '(: "{{" ($ (+ (~ ("}")))) "}}")))
        (wiki-word-rx
         (regexp '(: "[["
                     ($ (+ (~ ("]|"))))
                     (? "|" (* space) ($ (+ (~ ("]|")))))
                     "]]")))
        (wiki-url-rx
         (regexp '(: (or "http" "ftp") (? "s") ":"
                     (+ "/") (+ (or alnum ("-+.,_/%?&~=:")))
                     (or alnum ("-+_/%&~=")))))
        (wiki-named-url-rx
         (regexp '(: "["
                     ($ (or "http" "ftp") (? "s") ":"
                        (+ "/") (+ (or alnum ("-+.,_/%?&~=:")))
                        (or alnum ("-+_/%&~=")))
                     (? (+ " ")
                        ($ (* (~ ("]")))))
                     "]"))))
    (let ((rules
           `((,wiki-table-rx
              ,(lambda (m)
                 (append
                  (car
                   (html->sxml
                    (string-append
                     "<table "
                     (or (regexp-match-submatch m 1) "")
                     ">")))
                  (map (lambda (row)
                         (cons 'tr
                               (map (lambda (col)
                                      (cons 'td
                                            (wiki-parse-inline
                                             (string-trim col))))
                                    (regexp-split
                                     wiki-col-rx
                                     (string-trim-left
                                      row
                                      (lambda (c)
                                        (or (char-whitespace? c)
                                            (eqv? c #\|))))))))
                       (regexp-split
                        wiki-row-rx
                        (string-append "\n" (regexp-match-submatch m 2)))))))
             (,wiki-note-rx
              ,(lambda (m)
                 (cons 'note
                       (wiki-parse-inline (regexp-match-submatch m 1)))))
             (,wiki-word-rx
              ,(lambda (m)
                 (list 'wiki
                       (regexp-match-submatch m 1)
                       (regexp-match-submatch m 2))))
             (,wiki-named-url-rx
              ,(lambda (m)
                 (list 'url
                       (regexp-match-submatch m 1)
                       (or (regexp-match-submatch m 2)
                           (regexp-match-submatch m 1)))))
             (,wiki-url-rx
              ,(lambda (m)
                 (list 'url
                       (regexp-match-submatch m 0)
                       (regexp-match-submatch m 0))))
             (,wiki-bold-rx
              ,(lambda (m) (list 'b (regexp-match-submatch m 1))))
             (,wiki-italic-rx
              ,(lambda (m) (list 'i (regexp-match-submatch m 1))))
             (,wiki-uline-rx
              ,(lambda (m) (list 'u (regexp-match-submatch m 1))))
             (,wiki-strike-out-rx
              ,(lambda (m) (list 's (regexp-match-submatch m 1))))
             )))
      (lambda (str)
        (regexp-multi-fold rules str)))))

(define (string-count-prefix str ch)
  (let ((len (string-length str)))
    (let lp ((i 0))
      (if (and (< i len) (eqv? ch (string-ref str i)))
          (lp (+ i 1))
          i))))

(define wiki-parse
  (let ((wiki-hr-rx
         (regexp '(: bol (* space) (>= 4 "-") (* space) eol)))
        (wiki-header-rx
         (regexp '(: bol "=" ($ (+ "="))
                     ($ (+ (~ ("="))))
                     (+ "=") (* space) eol)))
        (wiki-list-level
         (lambda (str) (string-count-prefix str #\*))))
    (lambda (src)
      (let ((in (if (string? src) (open-input-string src) src)))
        (let parse ((res '())
                    (par '())
                    (list-level 0))
          (define (collect)
            (cond
             ((null? par)
              res)
             ((> list-level 0)
              `((ul ,@(map (lambda (x) (cons 'li (wiki-parse-inline x)))
                           (reverse par)))
                ,@res))
             (else
              `((p
                 ,@(reverse (drop-while string? par))
                 ,@(wiki-parse-inline
                    (string-join
                     (reverse (take-while string? par))
                     "\n")))
                ,@res))))
          (let ((line (read-line in)))
            (cond
             ((eof-object? line)
              (reverse (collect)))
             ((equal? "" line)
              (if (and (pair? par)
                       (pair? (car par))
                       (memq (caar par) '(h1 h2 h3 h4 h5 h6)))
                  (parse res par list-level)
                  (parse (collect) '() 0)))
             ((eqv? #\space (string-ref line 0))
              (let lp ((ls (list line)))
                (if (eqv? #\space (peek-char in))
                    (lp (cons (read-line in) ls))
                    (let* ((prefix
                            (fold (lambda (s p)
                                    (min (string-count-prefix s #\space)
                                         p))
                                  20 ls))
                           ;; strip up to 20 leading space chars
                           (ls (map (lambda (s) (substring s prefix)) ls)))
                      (parse
                       (cons (list 'code (string-join (reverse ls) "\n"))
                             (collect))
                       '()
                       0)))))
             ((regexp-matches? wiki-hr-rx line)
              (parse (cons (list 'hr) (collect)) '() 0))
             ((regexp-matches wiki-header-rx line)
              => (lambda (m)
                   (let* ((h (regexp-match-submatch m 1))
                          (depth (number->string (min (string-length h) 6))))
                     (parse
                      (collect)
                      (list (list (string->symbol (string-append "h" depth))
                                  (regexp-match-submatch m 2)))
                      0))))
             (else
              (let* ((level (wiki-list-level line))
                     (line (if (zero? level)
                               line
                               (string-trim-left (substring line level)))))
                (cond
                 ((= level list-level)
                  (parse res (cons line par) list-level))
                 (else
                  (parse (collect) (list line) level))))))))))))
