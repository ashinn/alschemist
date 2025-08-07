
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additional state information

(define-syntax define-state-variables
  (syntax-rules ()
    ((define-state-variables var ...)
     (begin
       (define var
         (make-state-variable 'var #f #f))
       ...))))

(define-state-variables
  expression? return? non-spaced-ops? no-wrap?
  expr-writer indent-space
  indent default-type dot op)

(define (py-in-expr proc) (with ((expression? #t)) (py-expr proc)))
(define (py-in-stmt proc) (with ((expression? #f)) (py-expr proc)))
(define (py-with-op new-op proc) (with ((op new-op)) proc))

(define (call-with-output-string proc)
  (let ((out (open-output-string)))
    (proc out)
    (let ((res (get-output-string out)))
      (close-output-port out)
      res)))

(define nl-str (call-with-output-string newline))
(define (make-nl-space n) (string-append nl-str (make-string n #\space)))
(define (make-space n) (make-string n #\space))

(define (display-to-string x)
  (if (string? x)
      x
      (call-with-output-string (lambda (out) (display x out)))))

(define (write-to-string x)
  (call-with-output-string (lambda (out) (write x out))))

(define (string-find/index str pred i)
  (string-cursor->index
   str
   (string-index str pred (string-index->cursor str i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; be smart about operator precedence

(define (py-op-precedence x)
  (if (string? x)
      (cond
        ((string=? x "."))
        ((string=? x "|") 65)
        ((string=? x "||") 75)
        ((string=? x "|=") 85)
        ((or (string=? x "+=") (string=? x "-=")) 85)
        (else (py-op-precedence (string->symbol x))))
      (case x
        ((zero) 0)
        ((paren bracket colon :) 5)
        ((**) 15)
        ((unary+ unary- ! ~ cast unary-* unary-& sizeof) 20)
        ((* / %) 30)
        ((+ -) 35)
        ((<< >>) 40)
        ((< > <= >=) 45)
        ((== !=) 50)
        ((&) 55)
        ((^) 60)
        ((&&) 70)
        ((? if) 80)
        ((= *= /= %= &= ^= <<= >>=) 85)
        ((comma) 90)
        ((not) 92)
        ((and) 93)
        ((or xor) 94)
        (else 95))))

(define (py-op< x y) (< (py-op-precedence x) (py-op-precedence y)))
(define (py-op<= x y) (<= (py-op-precedence x) (py-op-precedence y)))

(define (py-paren x)
  (each "(" x ")"))

(define (py-maybe-paren x-op x)
  (fn ((orig-op op))
    (let ((x (with ((op x-op)) x)))
      (if (py-op<= orig-op x-op)
          (py-paren x)
          x))))

(define (py-colon . o)
  (let ((from (and (pair? o) (car o)))
        (to (and (pair? o) (pair? (cdr o)) (cadr o))))
    (cond
     ((and from to)
      (each (py-expr from) ":" (py-expr to)))
     (from
      (each (py-expr from) ":"))
     (to
      (each ":" (py-expr to)))
     (else
      (each ":")))))

(define (py-slice seq . o)
  (py-wrap-stmt
   (each
    (py-expr seq)
    "[" (apply py-colon o) "]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default literals writer

(define (py-control-operator? x)
  (memq x '(if while for fun begin)))

(define (py-literal? x)
  (or (number? x) (string? x) (char? x) (boolean? x)))

(define (char->py-char c)
  (string-append "'" (py-escape-char c #\') "'"))

(define (py-escape-char c quote-char)
  (let ((n (char->integer c)))
    (if (<= 32 n 126)
        (if (or (eqv? c quote-char) (eqv? c #\\))
            (string #\\ c)
            (string c))
        (case n
          ((7) "\\a") ((8) "\\b") ((9) "\\t") ((10) "\\n")
          ((11) "\\v") ((12) "\\f") ((13) "\\r")
          (else (string-append "\\x" (number->string (char->integer c) 16)))))))

(define (py-format-number x)
  (if (and (integer? x) (exact? x))
      (fn (radix)
        (case radix
          ((16) (each "0x" (number->string x 16)))
          ((8) (each "0" (number->string x 8)))
          (else (each (number->string x)))))
      (each (number->string x))))

(define (py-format-string s)
  (each "\"" (each-in-list (py-string-escaped s)) "\""))

(define (py-string-escaped s)
  (let ((start (string-cursor-start s)))
    (let lp ((parts '()) (i (string-cursor-end s)))
      (let ((j (string-index-right s py-needs-string-escape? start i)))
        (cond
         ((string-cursor>? j start)
          (lp (cons (py-escape-char
                     (string-ref/cursor s (string-cursor-prev s j))
                     #\")
                    (cons (substring/cursors s j i) parts))
              (string-cursor-prev s j)))
         (else
          (cons (substring/cursors s start i) parts)))))))

(define (py-needs-string-escape? c)
  (if (<= 32 (char->integer c) 127) (memv c '(#\" #\\)) #t))

(define (py-literal x)
  (py-wrap-stmt
   (cond ((char? x) (each (char->py-char x)))
         ((boolean? x) (each (if x "True" "False")))
         ((number? x) (py-format-number x))
         ((string? x) (py-format-string x))
         ((or (null? x) (equal? x (if #f #f))) (each "None"))
         (else (each (write-to-string x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default expression generator

(define (py-expr/sexp x)
  (cond
   ((procedure? x)
    x)
   ((pair? x)
    (case (car x)
      ((if) (apply py-if (cdr x)))
      ((for) (apply py-for (cdr x)))
      ((while) (apply py-while (cdr x)))
      ((break) py-break)
      ((continue) py-continue)
      ((return) (apply py-return (cdr x)))
      ((yield) (apply py-yield (cdr x)))
      ((vector-ref)
       (py-wrap-stmt
        (each (py-expr (cadr x)) "[" (py-expr (caddr x)) "]")))
      ((vector-set!)
       (py= (py-in-expr
             (each (py-expr (cadr x)) "[" (py-expr (caddr x)) "]"))
            (py-expr (cadddr x))))
      ((%apply) (apply py-apply (cdr x)))
      ((%begin) (apply py-begin (cdr x)))
      ((%cond)
       (let lp ((ls (cdr x)) (res '()))
         (if (null? ls)
             (apply py-if (reverse res))
             (lp (cdr ls)
                 (cons (if (pair? (cddar ls))
                           (each-in-list (cdar ls))
                           (cadar ls))
                       (cons (caar ls) res))))))
      ((%comment) (apply py-comment (cdr x)))
      ((%enum) (apply py-enum (cdr x)))
      ((lambda %lambda) (apply py-lambda (cdr x)))
      ((%slice) (apply py-slice (cdr x)))
      ((&&) (apply py&& (cdr x)))
      ((+ - & * / % ** ! ~ ^ < > <= >= == != << >>
          = *= /= %= &= ^= >>= <<=)
       (apply py-op x))
      ((: %colon) (apply py-colon (cdr x)))
      ((bitwise-and bit-and) (apply py-op '& (cdr x)))
      ((bitwise-ior bit-or) (apply py-op "|" (cdr x)))
      ((bitwise-xor bit-xor) (apply py-op '^ (cdr x)))
      ((bitwise-not bit-not) (apply py-op '~ (cdr x)))
      ((arithmetic-shift) (apply py-op '<< (cdr x)))
      ((bitwise-ior= bit-or=) (apply py-op "|=" (cdr x)))
      ((%and) (apply py-op "&&" (cdr x)))
      ((%or) (apply py-op "||" (cdr x)))
      ((%. %field) (apply py-op "." (cdr x)))
      (else
       (cond
        ((eq? (car x) (string->symbol "."))
         (apply py-op "." (cdr x)))
        ((eq? (car x) (string->symbol "->"))
         (apply py-op "->" (cdr x)))
        ((eq? (car x) (string->symbol "++"))
         (apply py-op "++" (cdr x)))
        ((eq? (car x) (string->symbol "--"))
         (apply py-op "--" (cdr x)))
        ((eq? (car x) (string->symbol "+="))
         (apply py-op "+=" (cdr x)))
        ((eq? (car x) (string->symbol "-="))
         (apply py-op "-=" (cdr x)))
        (else (py-apply x))))))
   ((vector? x)
    (py-wrap-stmt
     (each "[" (joined py-expr (vector->list x) ", ") "]")))
   (else
    (py-literal x))))

(define (try-fitted2 proc fail)
  (fn (width (orig-output output))
    (let ((out (open-output-string)))
      (call-with-current-continuation
       (lambda (abort)
         ;; Modify output to accumulate to an output string port,
         ;; and escape immediately with failure if we exceed the
         ;; column width.
         (define (output* str)
           (fn (col)
             (let lp ((i 0) (col col))
               (let ((nli (string-find/index str #\newline i))
                     (len (string-length str)))
                 (if (< nli len)
                     (if (> (+ (- nli i) col) width)
                         (abort fail)
                         (lp (+ nli 1) 0))
                     (let ((col (+ (- len i) col)))
                       (cond
                        ((> col width)
                         (abort fail))
                        (else
                         (output-default str)))))))))
         (forked
          (with ((output output*)
                 (port out))
            proc)
          ;; fitted successfully
          (fn () (orig-output (get-output-string out)))))))))

(define (try-fitted proc . fail)
  (let lp ((proc proc) (ls fail))
    (if (null? ls)
        proc
        (try-fitted2 proc (lp (car ls) (cdr ls))))))

(define (py-apply ls)
  (py-wrap-stmt
   (with ((op 'comma))
     (each
      (py-expr (car ls))
      (let ((flat (with ((no-wrap? #t)) (joined py-expr (cdr ls) ", "))))
        (fn (no-wrap?)
          (if no-wrap?
              (py-paren flat)
              (py-paren
               (try-fitted
                flat
                (fn (col)
                  (let ((sep (string-append "," (make-nl-space col))))
                    (joined py-expr (cdr ls) sep))))))))))))

(define (py-expr x)
  (fn (expr-writer) ((or expr-writer py-expr/sexp) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comments

(define (make-comment-writer col output)
  (lambda (str)
    (let ((end (string-cursor-end str))
          (indent (string-append (make-nl-space col) "# ")))
      (let lp ((i (string-cursor-start str)))
        (let ((j (string-index str #\newline i)))
          (each
           (output (substring/cursors str i j))
           (if (string-cursor<? j end)
               (each (output indent)
                     (lp (string-cursor-next str j)))
               nothing)))))))

(define (py-comment . args)
  (fn (col (orig-output output))
    (each "# "
          (with ((output (make-comment-writer col orig-output)))
            (each-in-list args))
          nl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; indentation

(define (py-indent-string col . o)
  (make-space (max 0 (+ col (if (pair? o) (car o) 0)))))

;; (py-indent [offset])
(define (py-indent . o)
  (fn (indent-space)
    (displayed
     (make-space (max 0 (+ (or indent-space 4)
                           (if (pair? o) (car o) 0)))))))

(define (py-wrap-stmt x)
  (fn (expression? return?)
    (if expression?
        (py-expr x)
        (each (if return? "return " "")
              (py-in-expr (py-expr x))
              nl))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code blocks

;; A block of code indented to col.
(define (py-block col body0 . body)
  (let ((sep (each fl (py-indent-string col))))
    (if (null? body)
        (each (space-to col) (py-expr body0) fl)
        (fn ((orig-return? return?))
          (each
           (space-to col)
           (with ((return? #f))
             (joined py-expr (cons body0 (drop-right body 1)) sep))
           sep
           (with ((return? orig-return?))
             (py-wrap-stmt (last body)))
           fl)))))

(define (py-begin . body)
  (fn (col expression?)
    (cond
     ((null? body) nothing)
     (expression?
      (with ((no-wrap? #t))
        (joined py-expr body ", ")))
     (else
      (apply py-block col body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data structures

(define (py-class name . body)
  (fn (col indent)
    (let ((sig
           (if (pair? name)
               (each (car name) "(" (cadr name) ")")
               (displayed name))))
      (each "class " sig ":" nl
            (py-in-stmt (apply py-block (+ col (or indent 4)) body))
            fl))))

(define (py-enum name . vals)
  (define (py-enumerate-vals ls)
    (let lp ((ls ls) (last -1) (res '()))
      (cond
       ((null? ls)
        (reverse res))
       ((pair? (car ls))
        (lp (cdr ls)
            (max last (cadr (car ls)))
            (cons (apply py= (car ls)) res)))
       (else
        (lp (cdr ls)
            (+ last 1)
            (cons (py= (car ls) (+ last 1)) res))))))
  (apply py-class
         (list name 'Enum)
         (py-enumerate-vals vals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic control structures

(define (py-while check . body)
  (fn (col expression?)
    (each
     "while " (py-expr check) ":" nl
     (py-in-stmt (apply py-block col body))
     fl)))

(define (py-for vars iter . body)
  (fn (col expression?)
    (each
     "for " vars " in " (py-expr iter) ":" nl
     (py-in-stmt (apply py-block col body))
     fl)))

(define (py-def name params . body)
  (fn (col indent)
    (each "def " name "(" (py-in-expr (joined py-expr params ", ")) "):" nl
          (py-in-stmt (apply py-block (+ col (or indent 4)) body))
          fl)))

(define (py-lambda params expr)
  (fn ()
    (each "lambda " (py-in-expr params) ": " (py-in-expr expr) fl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized IF: allows multiple tail forms for if/else if/.../else
;; blocks.  A final ELSE can be signified with a test of #t or 'else,
;; or by simply using an odd number of expressions (by which the
;; normal 2 or 3 clause IF forms are special cases).

(define (py-if/stmt c p . rest)
  (fn (col indent)
    (let ((indent-str (make-string col #\space))
          (inner-col (+ col (or indent 4))))
      (let lp ((c c) (p p) (ls rest))
        (if (or (eq? c 'else) (eq? c #t))
            (if (not (null? ls))
                (error "forms after else clause in IF" c p ls)
                (each indent-str "else:" nl (py-block inner-col p)))
            (let ((tail (if (pair? ls)
                            (if (pair? (cdr ls))
                                (lp (car ls) (cadr ls) (cddr ls))
                                (lp 'else (car ls) '()))
                            fl)))
              (each (if (eq? ls rest) "if " (each indent-str "elif "))
                    (py-in-expr (py-expr c)) ":" nl
                    (py-block inner-col p)
                    tail)))))))

(define (py-if/expr c p . rest)
  (let lp ((c c) (p p) (ls rest))
    (cond
     ((or (eq? c 'else) (eq? c #t))
      (if (not (null? ls))
          (error "forms after else clause in IF" c p ls)
          (py-expr p)))
     ((pair? ls)
      (py-maybe-paren
       'if
       (with ((op 'if))
         (py-expr p) " if " (py-in-expr (py-expr c))
         " else "
         (if (pair? (cdr ls))
             (lp (car ls) (cadr ls) (cddr ls))
             (py-expr (car ls))))))
     (else
      (py-or (py-in-expr (py-expr c)) (py-expr p))))))

(define (py-if . args)
  (fn (expression?)
    (if expression?
        (apply py-if/expr args)
        (apply py-if/stmt args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other control flow

(define py-break
  (py-wrap-stmt (displayed "break")))
(define py-continue
  (py-wrap-stmt (displayed "continue")))
(define (py-return expr)
  (with ((return? #t))
    (py-expr expr)))
(define (py-yield expr)
  (py-wrap-stmt (each "yield " (py-expr expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operators

(define (py-op op first . rest)
  (if (null? rest)
      (py-unary-op op first)
      (apply py-binary-op op first rest)))

(define (py-binary-op op . ls)
  (define (lit-op? x) (or (py-literal? x) (symbol? x)))
  (let ((str (display-to-string op)))
    (py-wrap-stmt
     (py-maybe-paren
      op
      (if (or (equal? str ".") (equal? str "->"))
          (joined py-expr ls str)
          (let ((flat
                 (with ((no-wrap? #t))
                   (fn (non-spaced-ops?)
                     (joined py-expr
                             ls
                             (if (and (or non-spaced-ops?
                                          (member op '(: ** ":" "**")))
                                      (every lit-op? ls))
                                 str
                                 (string-append " " str " ")))))))
            (fn (no-wrap?)
              (if no-wrap?
                  flat
                  (try-fitted
                   flat
                   (fn (col)
                     (joined py-expr
                             ls
                             (each nl (make-space (+ 2 col)) str " ")
                             )))))))))))

(define (py-unary-op op x)
  (py-wrap-stmt
   (each (display-to-string op) (py-maybe-paren op (py-expr x)))))

;; some convenience definitions

(define (py+ . args) (apply py-op '+ args))
(define (py- . args) (apply py-op '- args))
(define (py* . args) (apply py-op '* args))
(define (py/ . args) (apply py-op '/ args))
(define (py% . args) (apply py-op '% args))
(define (py** . args) (apply py-op '** args))
(define (py& . args) (apply py-op '& args))
(define (py^ . args) (apply py-op '^ args))
(define (py~ . args) (apply py-op '~ args))
(define (py! . args) (apply py-op '! args))
(define (py: . args) (apply py-colon args))
(define (py&& . args) (apply py-op 'and args))
(define (py<< . args) (apply py-op '<< args))
(define (py>> . args) (apply py-op '>> args))
(define (py== . args) (apply py-op '== args))
(define (py!= . args) (apply py-op '!= args))
(define (py< . args) (apply py-op '< args))
(define (py> . args) (apply py-op '> args))
(define (py<= . args) (apply py-op '<= args))
(define (py>= . args) (apply py-op '>= args))
(define (py= . args) (apply py-op '= args))
(define (py+= . args) (apply py-op "+=" args))
(define (py-= . args) (apply py-op "-=" args))
(define (py*= . args) (apply py-op '*= args))
(define (py/= . args) (apply py-op '/= args))
(define (py%= . args) (apply py-op '%= args))
(define (py&= . args) (apply py-op '&= args))
(define (py^= . args) (apply py-op '^= args))
(define (py<<= . args) (apply py-op '<<= args))
(define (py>>= . args) (apply py-op '>>= args))
(define (py-bit-or . args) (apply py-op "|" args))
(define (py-or . args) (apply py-op 'or args))
(define (py-bit-or= . args) (apply py-op "|=" args))
