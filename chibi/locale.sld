
(define-library (chibi locale)
  (import (scheme base))
  (cond-expand
   ((library (srfi 130))
    (import (srfi 130)))
   (else
    (begin
      (define (string-cursor->index str i) i)
      (define (string-index->cursor str i) i)
      (define string-cursor? integer?)
      (define string-cursor<? <)
      (define string-cursor>? >)
      (define string-cursor=? =)
      (define string-cursor<=? <=)
      (define string-cursor>=? >=)
      (define string-cursor-ref string-ref)
      (define (string-cursor-start s) 0)
      (define string-cursor-end string-length)
      (define (string-cursor-next s i) (+ i 1))
      (define (string-cursor-prev s i) (- i 1))
      (define (substring-cursor s start . o)
        (substring s start (if (pair? o) (car o) (string-length s))))
      (define (string-pad str len ch)
        (if (< (string-length str) len)
            (string-append (make-string (- len (string-length str)) ch) str)
            str))
      (define (string-concatenate orig-ls . o)
        (let ((sep (if (pair? o) (car o) ""))
              (out (open-output-string)))
          (let lp ((ls orig-ls))
            (cond
             ((pair? ls)
              (if (and sep (not (eq? ls orig-ls)))
                  (write-string sep out))
              (write-string (car ls) out)
              (lp (cdr ls)))))
          (get-output-string out))))))
  (export locale? make-locale
          locale-language locale-script locale-region
          locale-variant locale-extensions
          locale->string string->locale
          locale->list list->locale)
  (include "locale.scm"))
