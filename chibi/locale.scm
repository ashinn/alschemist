
(define-record-type Locale
  (make-locale language script region variant extensions)
  locale?
  (language locale-language)
  (script locale-script)
  (region locale-region)
  (variant locale-variant)
  (extensions locale-extensions))

(define (locale->list locale)
  (list (locale-language locale)
        (locale-script locale)
        (locale-region locale)
        (locale-variant locale)
        (locale-extensions locale)))

(define (list->locale ls)
  (apply make-locale ls))

(define (string->locale str)
  (let ((end (string-cursor-end str)))
    (let lp ((from (string-cursor-start str))
             (to (string-cursor-start str))
             (lang #f)
             (script #f)
             (region #f)
             (variant #f))
      (cond
       ((and (string-cursor=? from to) (string-cursor>=? to end))
        (make-locale lang script region variant '()))
       ((or (string-cursor=? to end)
            (memv (string-ref/cursor str to) '(#\- #\_)))
        (let ((tag (substring/cursors str from to))
              (to2 (string-cursor-next str to)))
          (case (string-length tag)
            ((1)  ; extension
             (let ((end2 (string-cursor-prev str (string-cursor-prev str end))))
               (let lp ((from to2) (to to2) (t (string->symbol tag)) (ext '()))
                 (let* ((to2 (string-cursor-next str to))
                        (to3 (string-cursor-next str to2)))
                   (cond
                    ((or (eq? t 'x) (string-cursor>=? to end2))
                     (make-locale
                      lang script region variant
                      (reverse
                       (cons (cons t (substring/cursors str from end)) ext))))
                    ((and (eqv? #\- (string-ref/cursor str to))
                          (eqv? #\- (string-ref/cursor str to3)))
                     (let ((to4 (string-cursor-next str to3)))
                       (lp to4 to4
                           (string->symbol (substring/cursors str to2 to3))
                           (cons (cons t (substring/cursors str from to))
                                 ext))))
                    (else
                     (lp from to2 t ext)))))))
            ((2 3)  ; language or (optionally 3-digit numeric) region
             (cond
              (variant
               (error "locale language out of order: " tag " after" variant))
              ((not lang)
               (lp to2 to2 (string->symbol tag) script region variant))
              ((= 2 (string-length tag))
               (lp to2 to2 lang script (string->symbol tag) variant))
              ((string->number tag)
               => (lambda (n) (lp to2 to2 lang script n variant)))
              (else
               (error "3-letter region code not numeric" tag))))
            ((4)  ; script
             (if (or region variant)
                 (error "locale script out of order: " tag " after"
                        (or region variant))
                 (lp to2 to2 lang (string->symbol tag) region variant)))
            ((5 6 7 8)  ; language or variant
             (if lang
                 (lp to2 to2 lang script region (string->symbol tag))
                 (lp to2 to2 (string->symbol tag) script region variant)))
            (else (error "invalid language tag" tag)))))
       (else
        (lp from (string-cursor-next str to) lang script region variant)
        )))))

(define (locale->string locale)
  (string-concatenate
   `(,(symbol->string (locale-language locale))
     ,@(cond
        ((locale-script locale)
         => (lambda (script) `("-" ,(symbol->string script))))
        (else '()))
     ,@(cond
        ((locale-region locale)
         => (lambda (region)
              `("-" ,(if (number? region)
                         (string-pad (number->string region) 3 #\0)
                         (symbol->string region)))))
        (else '()))
     ,@(cond
        ((locale-variant locale)
         => (lambda (variant) `("-" ,(symbol->string variant))))
        (else '()))
     ,@(cond
        ((pair? (locale-extensions locale))
         (let lp ((ls (locale-extensions locale)) (res '()))
           (cond
            ((null? ls)
             (reverse res))
            (else
             (lp (cdr ls)
                 `(,(cdar ls) "-" ,(symbol->string (caar ls)) "-" ,@res))))))
        (else '())))))
