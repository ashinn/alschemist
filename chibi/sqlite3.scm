
;;> A high-level wrapper around sqlite3, with automatic resource
;;> management, iteration via fold and/or conversion to lists, and SQL
;;> statements as s-expressions.

;;> \section{Simple interface}

;;> The fundamental sqlite3 result iterator.  Executes \var{stmt-obj}
;;> on \var{db}, binding \var{vals} first if given.  Then iterates on
;;> the results, each time calling \var{kons} with two arguments,
;;> \var{stmt} and the current accumulator, which starts as
;;> \var{knil}.  \var{stmt-obj} can be a prepared statement, a string
;;> to parse into a statement, or an SSQL sexp.

(define (sqlite3-fold kons knil db stmt . vals)
  (let ((stmt* (cond ((string? stmt)
                      (sqlite3-prepare db stmt))
                     ((pair? stmt)
                      (sqlite3-prepare db (ssql->sql stmt)))
                     (else stmt))))
    (if (not stmt*)
        (error "not a sqlite3 statement" stmt (sqlite3-errmsg db)))
    (sqlite3-reset stmt*)
    (if (pair? vals)
        (apply sqlite3-bind-all stmt* vals))
    (let lp ((acc knil))
      (let ((res (sqlite3-step stmt*)))
        (if (or (eqv? res SQLITE_OK) (eqv? res SQLITE_DONE))
            acc
            (lp (kons stmt* acc)))))))

;;> Executes \var{stmt} on \var{db}, binding \var{vals} first if
;;> given.  Returns all results as a list, where each row is a vector.

(define (sqlite3-select db stmt . vals)
  (define (cons-columns stmt acc) (cons (sqlite3-columns stmt) acc))
  (reverse (apply sqlite3-fold cons-columns '() db stmt vals)))

;;> Executes \var{stmt} on \var{db}, similar to \scheme{sqlite3-exec}
;;> but binding \var{vals} first if given.  Returns an unspecified
;;> value.

(define (sqlite3-do db stmt . vals)
  (apply sqlite3-fold (lambda (stmt acc) acc) (if #f #f) db stmt vals))

;;> Executes \var{stmt} on \var{db}, binding \var{vals} first if
;;> given.  Returns the first column of the first result.

(define sqlite3-get
  (let* ((first (list 'first))
         (get-first
          (lambda (stmt acc) (if (eq? acc first) (sqlite3-column stmt 0) acc))))
    (lambda (db stmt . vals)
      (apply sqlite3-fold get-first first db stmt vals))))

;;> Example:
;;>
;;> \example{
;;> (let ((db (sqlite3-open ":memory:")))
;;>   (sqlite3-do db '(create (table animals)
;;>                           (columns (name (varchar 64))
;;>                                    (color (varchar 64))
;;>                                    (weight int))))
;;>   (sqlite3-do db '(insert (into animals)
;;>                           (columns name color weight)
;;>                           (values #("cat" "black" 3)
;;>                                   #("dog" "white" 5)
;;>                                   #("pink" "elephant" 3000))))
;;>   (sqlite3-select db '(select (columns name color)
;;>                               (from animals)
;;>                               (where (< weight 10))
;;>                               (order name))))
;;> }

;;> \section{SQL as S-expressions}

;;> Utility to convert an "ssql" sexp-based SQL syntax to an SQL
;;> string, similar to the Chicken Scheme ssql egg.

(define (ssql->sql ssql)
  (define ssql-infix
    '(union as like in and or escape = < > <= >= <>))
  (define ssql-prefix
    '(not distinct all select table limit offset))
  (define ssql-suffix
    '(asc desc))
  (define (write-ssql-infix ls sep out)
    (let lp ((ls ls) (first? #t))
      (when (pair? ls)
        (if (not first?) (write-string sep out))
        (write-ssql-expr (car ls) out)
        (lp (cdr ls) #f))))
  (define (write-ssql-columns ls sep out)
    (write-string "(" out)
    (let lp ((ls ls) (first? #t))
      (when (pair? ls)
        (if (not first?) (write-string sep out))
        (cond
         ((and (pair? (car ls)) (not (eq? 'as (caar ls))))
          (write-ssql-expr (caar ls) out)
          (write-string " " out)
          (write-ssql-expr (car (cdar ls)) out))
         (else (write-ssql-expr (car ls) out)))
        (lp (cdr ls) #f)))
    (write-string ")" out))
  (define (write-ssql-expr expr out)
    (cond
     ((pair? expr)
      (case (car expr)
        ((string-ci= string-ci<> string-ci< string-ci> string-ci<= string-ci>=)
         (write-ssql-infix (cdr expr) (substring (symbol->string (car expr)) 9) out)
         (write-string " COLLATE NOCASE" out))
        ((string-append)
         (write-ssql-infix (cdr expr) " || " out))
        ((!=)
         (write-ssql-infix (cdr expr) " <> " out))
        (else
         (cond
          ((memq (car expr) ssql-infix)
           (write-ssql-infix (cdr expr) (symbol->string (car expr)) out))
          ((memq (car expr) ssql-prefix)
           (write (car expr) out)
           (write-string " " out)
           (write-ssql-infix (cdr expr) ", " out))
          ((memq (car expr) ssql-suffix)
           (write-ssql-infix (cdr expr) ", " out)
           (write-string " " out)
           (write (car expr) out))
          (else
           (write (car expr) out)
           (write-string "(" out)
           (write-ssql-infix (cdr expr) ", " out)
           (write-string ")" out))))))
     ((vector? expr)
      (write-string "(" out)
      (write-ssql-infix (vector->list expr) ", " out)
      (write-string ")" out))
     ((string? expr)
      (write-string "'" out)
      (write-string (string-join (string-split expr "'") "''") out)
      (write-string "'" out))
     ((boolean? expr) (write-string (if expr "1" "0") out))
     (else (write expr out))))
  (let ((out (open-output-string))
        (insert? (and (pair? ssql) (eq? 'insert (car ssql))))
        (create? (and (pair? ssql) (eq? 'create (car ssql)))))
    (let lp ((ls ssql) (first? #t))
      (if (not first?) (write-string " " out))
      (when (pair? ls)
        (cond
         ((pair? (car ls))
          (let ((parens? (and insert? (eq? 'columns (caar ls)))))
            (if parens? (write-string "(" out))
            (if (not (eq? 'columns (caar ls))) (write (caar ls) out))
            (write-string (if (memq (caar ls) '(order group)) " by " " ") out)
            ((if (and create? (eq? 'columns (caar ls)))
                 write-ssql-columns
                 write-ssql-infix)
             (cdar ls)
             (if (eq? 'where (caar ls)) " and " ", ")
             out)
            (if parens? (write-string ")" out) " ")))
         (else
          (write-ssql-expr (car ls) out)))
        (lp (cdr ls) #f)))
    (write-string ";" out)
    (get-output-string out)))

(define-syntax sqlite3-let-columns
  (syntax-rules (as)
    ((sqlite3-let-columns stmt i () body)
     (begin . body))
    ((sqlite3-let-columns stmt i ((as name alias) . rest) body)
     (let ((alias (sqlite3-column stmt i)))
       (sqlite3-let-columns stmt (+ i 1) rest body)))
    ((sqlite3-let-columns stmt i ((func . val) . rest) body)
     (sqlite3-let-columns stmt (+ i 1) rest body))
    ((sqlite3-let-columns stmt i (name . rest) body)
     (let ((name (sqlite3-column stmt i)))
       (sqlite3-let-columns stmt (+ i 1) rest body)))))

;;> \macro{(sqlite3-lambda db query body ...)}

;;> Syntax for a procedure of n arguments, to applied to the given
;;> query, and the body evaluated.  If the query is of the form
;;> \scheme{(select (columns ...) ...)}, then the names of the columns
;;> are bound with their corresponding values in \var{body}.

(define-syntax sqlite3-lambda
  (syntax-rules (select columns)
    ((sqlite3-lambda db (select (columns name ...) . query) . body)
     (let ((stmt (sqlite3-prepare
                  db (ssql->sql `(select (columns name ...) . query)))))
       (lambda args
         (sqlite3-reset stmt)
         (if (pair? args)
             (apply sqlite3-bind-all stmt args))
         (sqlite3-step stmt)
         (sqlite3-let-columns stmt 0 (name ...) body))))
    ((sqlite3-lambda db query . body)
     (let ((stmt (sqlite3-prepare db (ssql->sql `query))))
       (lambda args
         (sqlite3-reset stmt)
         (if (pair? args)
             (apply sqlite3-bind-all stmt args))
         (sqlite3-step stmt)
         (begin . body))))))

;;> \macro{(sqlite3-loop db query ((var init step) ...) [result])}

;;> Syntax for a procedure of n arguments, to applied to the given
;;> query, which must be of the form \scheme{(select (columns ...)
;;> ...)}.  \var{var}s are initialized to their correspnding
;;> \var{init}s, and for each result of the query are updated to the
;;> \var{step}s, as with the \scheme{do} syntax.  Within the
;;> \var{step}s, the named columns are bound to the SQL results as in
;;> \scheme{sqlite3-lambda}.  When all rows have been processed,
;;> returns \var{result}, which defaults to the current values of all
;;> \var{var}s.

;;> Example:
;;>
;;> \example{
;;> (let ((db (sqlite3-open ":memory:")))
;;>   (sqlite3-do db '(create (table animals)
;;>                           (columns (name (varchar 64))
;;>                                    (color (varchar 64))
;;>                                    (weight int))))
;;>   (sqlite3-do db '(insert (into animals)
;;>                           (columns name color weight)
;;>                           (values #("cat" "black" 3)
;;>                                   #("dog" "white" 5)
;;>                                   #("pink" "elephant" 3000))))
;;>   (let ((f (sqlite3-loop db (select (columns name color)
;;>                                     (from animals)
;;>                                     (where (< weight ?))
;;>                                     (order (desc name)))
;;>              ((res '() (cons (string-append color " " name) res))))))
;;>     (f 10)))
;;> }

(define-syntax sqlite3-loop
  (syntax-rules (select columns)
    ((sqlite3-loop db (select (columns name ...) . query)
       ((var init step) ...))
     (sqlite3-loop db (select (columns name ...) . query)
       ((var init step) ...) (values var ...)))
    ((sqlite3-loop db (select (columns name ...) . query)
       ((var init step) ...) result)
     (let ((stmt (sqlite3-prepare
                  db (ssql->sql `(select (columns name ...) . query)))))
       (lambda args
         (sqlite3-reset stmt)
         (if (pair? args)
             (apply sqlite3-bind-all stmt args))
         (let lp ((var init) ...)
           (let ((res (sqlite3-step stmt)))
             (if (or (eqv? res SQLITE_OK) (eqv? res SQLITE_DONE))
                 result
                 (sqlite3-let-columns
                  stmt 0 (name ...)
                  ((lp step ...)))))))))
    ((sqlite3-loop db query . body)
     (syntax-error "sqlite3-loop only supports select queries" query))))

;;> \section{Dynamically Typed Binding Forms}

(define (display-to-string x)
  (if (string? x)
      x
      (let ((out (open-output-string)))
        (display x out)
        (get-output-string out))))

;;> Binds the \var{i}th argument of \var{stmt} to \var{val}.

(define (sqlite3-bind stmt i val)
  (cond
   ((boolean? val) (sqlite3-bind-int stmt i (if val 1 0)))
   ((exact-integer? val) (sqlite3-bind-int stmt i val))
   ((real? val) (sqlite3-bind-double stmt i val))
   ((bytevector? val) (sqlite3-bind-blob stmt i val))
   (else (sqlite3-bind-text stmt i (display-to-string val)))))

;;> Binds the arguments of \var{stmt} to \var{vals}, in order.

(define (sqlite3-bind-all stmt . vals)
  (let lp ((i 1) (vals vals))
    (when (pair? vals)
      (sqlite3-bind stmt i (car vals))
      (lp (+ i 1) (cdr vals)))))

;;> Returns the \var{col}th column of the current result of \var{stmt}.

(define (sqlite3-column stmt col)
  (let ((type (sqlite3-column-type stmt col)))
    (cond
     ((eqv? type SQLITE_INTEGER) (sqlite3-column-int stmt col))
     ((eqv? type SQLITE_FLOAT) (sqlite3-column-double stmt col))
     ((eqv? type SQLITE_NULL) 'NULL)
     ((eqv? type SQLITE_BLOB) (sqlite3-column-blob stmt col))
     (else (sqlite3-column-text stmt col)))))

;;> Returns all columns of the current result of \var{stmt} as a vector.

(define (sqlite3-columns stmt)
  (let* ((len (sqlite3-column-count stmt))
         (res (make-vector len)))
    (let lp ((i (- len 1)))
      (cond
       ((negative? i)
        res)
       (else
        (vector-set! res i (sqlite3-column stmt i))
        (lp (- i 1)))))))
