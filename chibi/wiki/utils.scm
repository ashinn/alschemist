
(define (get-file-cached-url url path)
  ;; TODO: also check modified time with HEAD once per day or so
  (if (not (file-exists? path))
      (http-get-to-file url path))
  path)

(define (transform-url-html proc url word words-dir)
  (guard (exn (else ""))
    (let* ((uri (if (string? url) (string->path-uri 'http url) url))
           (prefix0 (uri->string (uri-with-path uri "/")))
           (prefix (substring prefix0 0 (- (string-length prefix0) 1)))
           (dir (path-directory (or (uri-path uri) "/")))
           (path (string-append words-dir "/" word ".cache")))
      (sxml->xml
       (html-adjust-relative
        (proc
         (call-with-input-file (get-file-cached-url url path)
           html->sxml))
        prefix
        dir)))))

(define (get-title url word words)
  (transform-url-html html-title url word words))

(define (get-summary url word words)
  (transform-url-html html-summary url word words))

(define paren-rx
  (regexp '(: (? (" _")) "(" (+ (~ (")"))) ")")))

;; simple single char version
(define (string-translate str from to)
  (string-map (lambda (ch) (if (eqv? ch from) to ch)) str))

(define (wiki-word-display str)
  (let* ((i (string-find-right str #\:))
         (str (if (string-cursor>? i (string-cursor-start str))
                  (substring-cursor str i)
                  str)))
    (regexp-replace paren-rx (string-translate str #\_ #\space) "")))

(define (wiki-code-links x words aliases)
  ;; XXXX handle other languages
  (let* ((r5rs (cond ((assoc "r5rs:" aliases) => cdr) (else '())))
         (prefix (and (pair? r5rs) (car r5rs)))
         (docs (if (pair? r5rs) (cdr r5rs) '())))
    (let lp ((x x))
      (if (not (pair? x))
          x
          (if (eq? 'span (car x))
              (let* ((attrs? (and (pair? (cdr x)) (pair? (cadr x))
                                  (eq? '@ (car (cadr x)))))
                     (name (if attrs? (cddr x) (cdr x))))
                (if (and (pair? name) (every string? name))
                    (let ((cell (assq (string->symbol (string-join name))
                                      docs)))
                      (if cell
                          `(span ,@(if attrs? (list (cadr x)) '())
                                 (wiki ,(string-append prefix (cadr cell))
                                       ,@name
                                       ,@(cddr cell)))
                          x))
                    (cons (lp (car x)) (lp (cdr x)))))
              (cons (lp (car x)) (lp (cdr x))))))))

(define (wiki-sugar x words aliases)
  (define (sugar x) (wiki-sugar x words aliases))
  (if (not (pair? x))
      x
      (case (car x)
        ((wiki)
         (let ((word (cadr x)))
           (cond
            ((find (lambda (a) (string-prefix? (car a) word)) aliases)
             => (lambda (a)
                  (let* ((rel-word (substring word (string-length (car a))))
                         (name (if (and (pair? (cddr x)) (third x))
                                   (third x)
                                   rel-word)))
                    (cond
                     ((and (pair? (cddr a)) (pair? (third a)))
                      ;; inline docs like r5rs:
                      (let ((cell (assq (string->symbol rel-word) (cddr a))))
                        (if (not cell)
                            (list 'wiki (cadr a) name)
                            (let* ((url (string-append (cadr a)
                                                       (or (cadr cell) "")))
                                   (doc (and (pair? (cddr cell))
                                             (third cell))))
                              (list 'wiki url name
                                    (and (not (equal? "" doc)) doc))))))
                     (else
                      (let* ((url (string-append
                                   (cadr a)
                                   (string-translate rel-word #\space #\_)))
                             (doc (case (and (pair? (cddr a)) (third a))
                                    ((summary) (get-summary url word words))
                                    ((title) (get-title url word words))
                                    (else ""))))
                        (list 'wiki url name
                              (and doc
                                   (not (equal? "" doc))
                                   (html-escape doc)))))))))
            ((string-prefix? "image:" word)
             `(img (@ (src ,(substring word 6)))))
            (else
             x))))
        ((url)
         `(a (@ (href ,(cadr x))) ,(third x)))
        ((code)
         (let ((code (highlight (cadr x))))
           `(code (pre ,(wiki-code-links code words aliases)))))
        (else
         (cond
          ((pair? (car x))
           (map sugar x))
          ((and (pair? (cdr x)) (pair? (cadr x)) (eq? '@ (car (cadr x))))
           (cons (car x) (cons (cadr x) (map sugar (cddr x)))))
          (else
           (cons (car x) (map sugar (cdr x)))))))))
