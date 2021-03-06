#!/usr/bin/env chibi-scheme

;; Extract character script mappings
;;
;; Usage:
;;   extract-scripts.scm Scripts.txt > out

(import (scheme small) (scheme list) (srfi 95) (srfi 130)
        (chibi iset) (chibi iset optimize))

(define (warn . args)
  (let ((err (current-error-port)))
    (for-each
     (lambda (x) (if (not (string? x)) (display " " err)) (display x err))
     args)
    (newline err)))

(define (symbol-append a b)
  (string->symbol (string-append (symbol->string a) (symbol->string b))))

(define (write-scripts ls out)
  (let lp ((ls ls) (start 0) (end 0) (script 'common) (res '()))
    (cond
     ((null? ls)
      (let ((res (reverse (cons (cons end script) res))))
        (write
         `(define range-ends ,(list->vector (map car res))) out)
        (newline out)
        (newline out)
        (write
         `(define range-scripts ,(list->vector (map cdr res))) out)
        (newline out)
        (newline out)
        (let ((scripts (sort (delete-duplicates (map cdr res)))))
          (for-each
           (lambda (script)
             (let lp ((ls res) (from 0) (ranges '()))
               (cond
                ((null? ls)
                 (write `(define ,(symbol-append 'char-set: script)
                           ,(iset->code
                             (iset-balance
                              (iset-optimize (apply iset-union ranges))))))
                 (newline))
                ((eq? script (cdar ls))
                 (lp (cdr ls)
                     (+ 1 (caar ls))
                     (cons (make-iset from (caar ls)) ranges)))
                (else
                 (lp (cdr ls) (+ 1 (caar ls)) ranges)))))
           scripts)
          (newline)
          (write
           `(define (script->char-set script)
              (case script
                ,@(map (lambda (s) `((,s) ,(symbol-append 'char-set: s)))
                       scripts))))
          (newline out))))
     (else
      (let ((lo (caar ls))
            (hi (cadr (car ls)))
            (script2 (cadr (cdar ls))))
        (cond
         ((eq? script script2)
          (lp (cdr ls) start hi script res))
         (else
          (lp (cdr ls) lo hi script2 (cons (cons end script) res)))))))))

;;0000..001F    ; Common # Cc  [32] <control-0000>..<control-001F>

(define (extract-scripts in out)
  (define (string-trim-comment str delim)
    (car (string-split str delim 2)))
  (display ";; auto-generated by extract-scripts.scm\n\n" out)
  (let lp ((ls '()))
    (let ((line (read-line in)))
      (cond
       ((eof-object? line)
        (write-scripts (sort ls < car) out))
       ((or (equal? line "") (eqv? #\# (string-ref line 0)))
        (lp ls))
       (else
        (let ((fields (map string-trim
                           (string-split (string-trim-comment line "#") ";"))))
          (cond
           ((< (length fields) 2)
            (warn "invalid Scripts.txt line: " line)
            (lp ls))
           (else
            (let* ((range (string-split (first fields) ".."))
                   (lo (string->number (car range) 16))
                   (hi (if (pair? (cdr range))
                           (string->number (cadr range) 16)
                           lo))
                   (script2 (string->symbol
                             (string-downcase
                              (string-trim-both
                               (string-join (string-split (second fields) "_")
                                            "-"))))))
              (cond
               ((not (and (integer? lo) (integer? hi)))
                (warn "invalid Scripts.txt line: " line lo hi)
                (lp ls))
               (else
                (lp (cons (list lo hi script2) ls)))))))))))))

(let ((args (command-line)))
  (let lp ((ls (cdr args))
           (out (current-output-port)))
    (cond
     ((and (pair? ls) (not (equal? "" (car ls)))
           (eqv? #\- (string-ref (car ls) 0)))
      (cond
       (else
        (error "unknown option: " (car ls)))))
     (else
      (if (or (< (length ls) 1) (equal? "-" (car ls)))
          (extract-scripts (current-input-port) out)
          (call-with-input-file (car ls)
            (lambda (in) (extract-scripts in out))))
      (close-output-port out)))))
