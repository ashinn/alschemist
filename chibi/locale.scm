
;;> A lightweight library for representing locale information and
;;> serializing to and from strings.

;;> A Locale record is an immutable object representing the
;;> information in IETF BCP 47.  This can be used for distinctions
;;> necessary in written and spoken language, but does not provide the
;;> full locale information specified by POSIX LC_* variables such as
;;> collation and numeric and time formatting, except for reasonable
;;> defaults that can be inferred from the region.

(define-record-type Locale
  (%make-locale language script region variant extensions)
  locale?
  (language locale-language)
  (script locale-script)
  (region locale-region)
  (variant locale-variant)
  (extensions locale-extensions))

;;> \procedure{(make-locale language [region variant script extensions])}
;;>
;;> Returns a new locale object for the given language, with optional
;;> region, variant, script and extensions.  The \var{language} should
;;> be a 2 or 3 letter symbol such as \scheme{'en} or \scheme{'ain}.
;;> The \var{region} is an ISO 3166 code, either an uppercased 2
;;> letter symbol such as \scheme{'US} or \scheme{'FR} or a 3 digit
;;> code from ISO 3166-1 or UN M49 such as \scheme{029} (Caribbean).
;;> The \var{variant} is 5-8 letter language variant such as
;;> \scheme{'polyton} for Polytonic (Ancient) Greek.  The \var{script}
;;> is a 4 letter titlecased symbol such as \scheme{'Latn} (Latin),
;;> \scheme{'Cyrl} (Cyrillic), or \scheme{'Jpan} (Han + Hiragana +
;;> Katakana).  The \var{extensions} are an alist whose keys are
;;> single letter symbols (currently only \scheme{'u} and \scheme{'x}
;;> are defined) with subtag string values.
;;>
;;> Note the order of the arguments differs from the serialized string
;;> representation (moving \var{script} after \var{region} and
;;> \var{variant}) to make the common case construction more
;;> convenient.
;;>
;;> Example:
;;> \example{(locale->string (make-locale 'ja 'JP))}
(define (make-locale language . o)
  (let* ((region (and (pair? o) (car o)))
         (o (if (pair? o) (cdr o) '()))
         (variant (and (pair? o) (car o)))
         (o (if (pair? o) (cdr o) '()))
         (script (and (pair? o) (car o)))
         (o (if (pair? o) (cdr o) '()))
         (extensions (if (and (pair? o) (pair? (cdr o))) (cadr o) '())))
    (%make-locale language script region variant extensions)))

;;> Returns \scheme{#t} iff all components of \var{a} and \var{b} are equal.
(define (locale= a b)
  (and (eq? (locale-language a) (locale-language b))
       (eq? (locale-script a) (locale-script b))
       (eq? (locale-region a) (locale-region b))
       (eq? (locale-variant a) (locale-variant b))
       (equal? (locale-extensions a) (locale-extensions b))))

;;> Returns \scheme{#t} iff \var{a} includes \var{b}, i.e. it \var{a}
;;> is a more general locale, differing only in components specified
;;> in \var{b} which are unspecific in \var{a}.
(define (locale-includes? a b)
  (and (or (not (locale-language a))
           (eq? (locale-language a) (locale-language b)))
       (or (not (locale-script a))
           (eq? (locale-script a) (locale-script b)))
       (or (not (locale-region a))
           (eq? (locale-region a) (locale-region b)))
       (or (not (locale-variant a))
           (eq? (locale-variant a) (locale-variant b)))
       (or (not (pair? (locale-extensions a)))
           (equal? (locale-extensions a) (locale-extensions b)))))

;;> Returns the next more general locale than \var{locale} by ablating
;;> one of its components, or \scheme{#f} if locale can't be made more
;;> general (i.e. is already the root).  The order of generalization
;;> follows that of the serialized string representation, first
;;> removing the extensions if present, then the variant, then the
;;> region, then script.
(define (locale-generalize locale)
  (let ((lang (locale-language locale))
        (script (locale-script locale))
        (region (locale-region locale))
        (variant (locale-variant locale))
        (extensions (locale-extensions locale)))
    (cond
     ((and (not lang) (not script) (not region) (not variant)
           (not (pair? extensions)))
      #f)
     ((pair? extensions) (%make-locale lang script region variant '()))
     (variant (%make-locale lang script region #f '()))
     (region (%make-locale lang script #f #f '()))
     (script (%make-locale lang #f #f #f '()))
     (else (%make-locale #f #f #f #f '())))))

;;> Returns the Locale record corresponding to the given serialized
;;> string representation, signalling an error on invalid format.
;;> Allows underscores as well as hyphens as tag separators.  Note
;;> that the LANG environment variable should not be passed to this
;;> directly as it typically has a .<encoding> suffix.
;;>
;;> Example:
;;> \example{(locale-language (string->locale "ja-JP"))}
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
        (%make-locale lang script region variant '()))
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
                     (%make-locale
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

;;> Returns the serialized string representation of \var{locale}.
(define (locale->string locale)
  (string-concatenate
   `(,(if (locale-language locale) (symbol->string (locale-language locale)) "")
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
