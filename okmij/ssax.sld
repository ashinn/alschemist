(define-library (okmij ssax)
  (export ssax:read-char-data ssax:read-attributes
          ssax:read-external-id ssax:scan-Misc ssax:resolve-name
          ssax:make-parser ssax:make-pi-parser ssax:make-elem-parser
          ssax:xml->sxml ssax:warn
          xml-token-kind xml-token-head)
  (import (scheme base) (scheme char) (scheme cxr) (scheme write) (srfi 1))
  (begin
    (define char-newline #\newline)
    (define char-return #\return)
    (define char-tab #\tab)
    (define char->ascii char->integer)
    (define ascii->char integer->char)
    (define ucscode->char integer->char)
    (define (parser-error port msg . specialising-msg*)
      (apply error msg specialising-msg*))
    (define (ssax:warn port msg . other-msg)
      (for-each (lambda (x) (display x (current-error-port)))
                `("\nWarning: " msg ,@other-msg "\n")))
    (define (call-with-input-string str proc)
      (let* ((in (open-input-string str))
             (res (proc str)))
        (close-input-port in)
        res))
    ;; we only need these from SRFI 13
    (define (string-concatenate/shared los)
      (apply string-append los))
    (define (string-concatenate-reverse los)
      (apply string-append (reverse los)))
    (define string-concatenate-reverse/shared string-concatenate-reverse)
    (define (string-index str what . o)
      (let ((pred (if (char? what) (lambda (ch) (eqv? ch what)) what))
            (end (if (and (pair? o) (pair? (cdr o)))
                     (cadr o)
                     (string-length str))))
        (let lp ((i (if (pair? o) (car o) 0)))
          (and (< i end)
               (if (pred (string-ref str i))
                   i
                   (lp (+ i 1))))))))
  (include "myenv-scm.scm"
           "input-parse.scm"
           "look-for-str.scm"
           "SSAX.scm"))
