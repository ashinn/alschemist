;; html-summary.scm -- utils to summarize html documents
;; Copyright (c) 2009-2015 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Extracts the <title> tag from the given sxml if present.
(define (html-title x)
  (and (pair? x)
       (if (eq? 'title (car x))
           x
           (and (not (eq? '@ (car x)))
                (or (html-title (car x)) (html-title (cdr x)))))))

;;> Returns true iff the sxml contains non-whitespace text.
(define (html-non-empty? x)
  (or (and (string? x) (string-any (lambda (c) (not (char-whitespace? c))) x))
      (and (pair? x)
           (any html-non-empty? (sxml-body x)))))

;;> Returns the first <p> tag from the given sxml.
(define (html-first-para x)
  (and (pair? x)
       (if (eq? 'p (car x))
           (and (html-non-empty? x)
                x)
           (or (html-first-para (car x))
               (html-first-para (cdr x))))))

;;> Returns the first "." terminated sentence from the given sxml.
(define (html-first-sentence x)
  (car
   (let lp1 ((x x))
     (cond
      ((string? x)
       (let ((i (string-contains x ". ")))
         (cond
          (i (cons (substring x 0 (+ i 1)) #t))
          ((string-suffix? "." x) (cons x #t))
          (else (cons x #f)))))
      ((pair? x)
       (let lp2 ((x x) (res '()))
         (if (not (pair? x))
             (cons (reverse res) #f)
             (let ((y (lp1 (car x))))
               (if (cdr y)
                   (cons (reverse (cons (car y) res)) #t)
                   (lp2 (cdr x) (cons (car y) res)))))))
      (else
       (cons x #f))))))

;;> Grabs the first sentence of the first paragraph in the document,
;;> removing formatting and parentheticals.
(define (html-summary sxml)
  (let ((p (html-first-para sxml)))
    (and p
         (html-flatten-spans
          (html-first-sentence
           (html-remove-wiki-help
            (html-remove-class
             (html-text-only p))))))))

(define (html-flatten-one ls)
  (let lp ((ls ls) (buf '()) (res '()))
    (define (collect)
      (if (null? buf) res (cons (string-join (reverse buf)) res)))
    (cond ((null? ls) (reverse (collect)))
          ((string? (car ls)) (lp (cdr ls) (cons (car ls) buf) res))
          ((and (pair? (car ls)) (eq? 'span (caar ls)))
           (lp (append (sxml-body (car ls)) (cdr ls)) buf res))
          (else (lp (cdr ls) '() (cons (car ls) (collect)))))))

(define (html-flatten-spans x)
  (if (not (pair? x))
      x
      (let ((ls (html-flatten-one (map html-flatten-spans (sxml-body x)))))
        (if (and (eq? 'span (car x)) (or (null? ls) (equal? ls "")))
            ""
            (cons (car x)
                  (if (and (pair? (cdr x)) (pair? (cadr x))
                           (eq? '@ (car (cadr x))))
                      (cons (cadr x) ls)
                      ls))))))

(define (html-text-only x)
  (match x
    (('a ('@ attrs ...) . ls)
     (let ((ls (delete "" (map html-text-only ls))))
       (if (or (null? ls) (and (null? (cdr ls)) (equal? "" (car ls))))
           ""
           (cons 'a (cons (cons '@ attrs) ls)))))
    (((or 'a 'b 'u 'i 's 'sub 'sup 'ul 'ol 'li 'p) . ls)
     (let ((ls (delete "" (map html-text-only ls))))
       (if (or (null? ls) (and (null? (cdr ls)) (equal? "" (car ls))))
           ""
           (cons (car x) ls))))
    (((or 'font 'span 'pre 'code) . ls)
     (let ((ls (delete "" (map html-text-only ls))))
       (cond
        ((null? ls)
         "")
        ((null? (cdr ls))
         (car ls))
        (else
         (cons 'span ls)))))
    (_
     (if (string? x) x ""))))

(define (question-mark-or-bracket-n? x)
  (let ((y (string-trim (html-strip (sxml->xml x)))))
    (or (equal? y "")
        (equal? y "?")
        (and (eqv? (string-ref y 0) #\[)
             (eqv? (string-ref y (- (string-length y) 1)) #\])
             (string->number (substring y 1 (- (string-length y) 1)))))))

;;> Removes ? help notes and [n] refs from wikipedia pages.
(define (html-remove-wiki-help x)
  (if (not (pair? x))
      x
      (match x
        (('sup ls ...)
         (if (question-mark-or-bracket-n? x)
             ""
             x))
        (_
         (cons (html-remove-wiki-help (car x))
               (html-remove-wiki-help (cdr x)))))))

(define (html-remove-class x)
  (if (not (pair? x))
      x
      (match x
        (('span ('@ attrs ...) body)
         (html-remove-class body))
        ((tag ('@ attrs ...) . body)
         `(,tag (@ ,@(assq-delete 'class attrs))
                ,@(map html-remove-class body)))
        ((ls ...)
         (map html-remove-class ls))
        (_
         x))))

(define (sxml-attributes x)
  (if (and (pair? (cdr x)) (pair? (cadr x)) (eq? '@ (car (cadr x))))
      (cdr (cadr x))
      '()))

(define (sxml-body x)
  (if (and (pair? (cdr x)) (pair? (cadr x)) (eq? '@ (car (cadr x))))
      (cddr x)
      (cdr x)))

(define (assq-delete key ls)
  (cond ((null? ls) '())
        ((eq? key (caar ls)) (assq-delete key (cdr ls)))
        (else (cons (car ls) (assq-delete key (cdr ls))))))

(define (sxml-update-attribute attrs key value)
  (cons (list key value) (assq-delete key attrs)))

(define (html-adjust-relative x prefix . o)
  (let ((rel-prefix (string-append prefix (if (pair? o) (car o) ""))))
    (let adjust ((x x))
      (if (not (pair? x))
          x
          (if (and (eq? 'a (car x)))
              (let* ((attrs (sxml-attributes x))
                     (href (cond ((assq 'href attrs) => cadr) (else #f))))
                (if (or (not href) (string->uri href))
                    x
                    (let* ((url (string-append
                                 (if (and (not (equal? "" href))
                                          (eqv? #\/ (string-ref href 0)))
                                     prefix
                                     rel-prefix)
                                 (or href "")))
                           (attrs (sxml-update-attribute attrs 'href url)))
                      `(a (@ ,@attrs) ,@(sxml-body x)))))
              (cons (adjust (car x))
                    (adjust (cdr x))))))))
