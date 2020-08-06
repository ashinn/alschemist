; 			My Standard Scheme "Prelude"
; $Id: myenv-scm.scm,v 1.8 2004/11/03 22:45:29 oleg Exp $

; Frequently-occurring syntax-rule macros

; A symbol? predicate at the macro-expand time
;	symbol?? FORM KT KF
; FORM is an arbitrary form or datum
; expands in KT if FORM is a symbol (identifier), Otherwise, expands in KF

(define-syntax symbol??
  (syntax-rules ()
    ((symbol?? (x . y) kt kf) kf)	; It's a pair, not a symbol
    ((symbol?? #(x ...) kt kf) kf)	; It's a vector, not a symbol
    ((symbol?? maybe-symbol kt kf)
      (let-syntax
	((test
	   (syntax-rules ()
	     ((test maybe-symbol t f) t)
	     ((test x t f) f))))
	(test abracadabra kt kf)))))

; A macro-expand-time memv function for identifiers
;	id-memv?? FORM (ID ...) KT KF
; FORM is an arbitrary form or datum, ID is an identifier.
; The macro expands into KT if FORM is an identifier, which occurs
; in the list of identifiers supplied by the second argument.
; All the identifiers in that list must be unique.
; Otherwise, id-memv?? expands to KF.
; Two identifiers match if both refer to the same binding occurrence, or
; (both are undefined and have the same spelling).

; (id-memv??			; old code. 
;   (syntax-rules ()
;     ((_ x () kt kf) kf)
;     ((_ x (y . rest) kt kf)
;       (let-syntax
; 	((test 
; 	   (syntax-rules (y)
; 	     ((test y _x _rest _kt _kf) _kt)
; 	     ((test any _x _rest _kt _kf)
; 	       (id-memv?? _x _rest _kt _kf)))))
; 	(test x x rest kt kf)))))


(define-syntax id-memv??
  (syntax-rules ()
    ((id-memv?? form (id ...) kt kf)
      (let-syntax
	((test
	   (syntax-rules (id ...)
	     ((test id _kt _kf) _kt) ...
	     ((test otherwise _kt _kf) _kf))))
	(test form kt kf)))))

; Test cases
; (id-memv?? x (a b c) #t #f)
; (id-memv?? a (a b c) 'OK #f)
; (id-memv?? () (a b c) #t #f)
; (id-memv?? (x ...) (a b c) #t #f)
; (id-memv?? "abc" (a b c) #t #f)
; (id-memv?? x () #t #f)
; (let ((x 1))
;   (id-memv?? x (a b x) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (a x b) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (x a b) 'OK #f))

; Commonly-used CPS macros
; The following macros follow the convention that a continuation argument
; has the form (k-head ! args ...)
; where ! is a dedicated symbol (placeholder).
; When a CPS macro invokes its continuation, it expands into
; (k-head value args ...)
; To distinguish such calling conventions, we prefix the names of
; such macros with k!

(define-syntax k!id			; Just the identity. Useful in CPS
  (syntax-rules ()
    ((k!id x) x)))

; k!reverse ACC (FORM ...) K
; reverses the second argument, appends it to the first and passes
; the result to K

(define-syntax k!reverse
  (syntax-rules (!)
    ((k!reverse acc () (k-head ! . k-args))
      (k-head acc . k-args))
    ((k!reverse acc (x . rest) k)
      (k!reverse (x . acc) rest k))))


; (k!reverse () (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1)
; (k!reverse (x) (1 2 () (4 5)) '!) ;==> '((4 5) () 2 1 x)
; (k!reverse (x) () '!) ;==> '(x)


; assert the truth of an expression (or of a sequence of expressions)
;
; syntax: assert ?expr ?expr ... [report: ?r-exp ?r-exp ...]
;
; If (and ?expr ?expr ...) evaluates to anything but #f, the result
; is the value of that expression.
; If (and ?expr ?expr ...) evaluates to #f, an error is reported.
; The error message will show the failed expressions, as well
; as the values of selected variables (or expressions, in general).
; The user may explicitly specify the expressions whose
; values are to be printed upon assertion failure -- as ?r-exp that
; follow the identifier 'report:'
; Typically, ?r-exp is either a variable or a string constant.
; If the user specified no ?r-exp, the values of variables that are
; referenced in ?expr will be printed upon the assertion failure.


(define-syntax assert
  (syntax-rules ()
    ((assert _expr . _others)
     (letrec-syntax
       ((write-report
	  (syntax-rules ()
			; given the list of expressions or vars,
			; create a cerr form
	    ((_ exprs prologue)
	      (k!reverse () (cerr . prologue)
		(write-report* ! exprs #\newline)))))
	 (write-report*
	   (syntax-rules ()
	     ((_ rev-prologue () prefix)
	       (k!reverse () (nl . rev-prologue) (k!id !)))
	     ((_ rev-prologue (x . rest) prefix)
	       (symbol?? x
		 (write-report* (x ": " 'x #\newline . rev-prologue) 
		   rest #\newline)
		 (write-report* (x prefix . rev-prologue) rest "")))))
	  
			; return the list of all unique "interesting"
			; variables in the expr. Variables that are certain
			; to be bound to procedures are not interesting.
	 (vars-of 
	   (syntax-rules (!)
	     ((_ vars (op . args) (k-head ! . k-args))
	       (id-memv?? op 
		 (quote let let* letrec let*-values lambda cond quasiquote
		   case define do assert)
		 (k-head vars . k-args) ; won't go inside
				; ignore the head of the application
		 (vars-of* vars args (k-head ! . k-args))))
		  ; not an application -- ignore
	     ((_ vars non-app (k-head ! . k-args)) (k-head vars . k-args))
	     ))
	 (vars-of*
	   (syntax-rules (!)
	     ((_ vars () (k-head ! . k-args)) (k-head vars . k-args))
	     ((_ vars (x . rest) k)
	       (symbol?? x
		 (id-memv?? x vars
		   (vars-of* vars rest k)
		   (vars-of* (x . vars) rest k))
		 (vars-of vars x (vars-of* ! rest k))))))

	 (do-assert
	   (syntax-rules (report:)
	     ((_ () expr)			; the most common case
	       (do-assert-c expr))
	     ((_ () expr report: . others) ; another common case
	       (do-assert-c expr others))
	     ((_ () expr . others) (do-assert (expr and) . others))
	     ((_ exprs)
	       (k!reverse () exprs (do-assert-c !)))
	     ((_ exprs report: . others)
	       (k!reverse () exprs (do-assert-c ! others)))
	     ((_ exprs x . others) (do-assert (x . exprs) . others))))

	 (do-assert-c
	   (syntax-rules ()
	     ((_ exprs)
	       (or exprs
		 (begin (vars-of () exprs
			  (write-report ! 
			    ("failed assertion: " 'exprs nl "bindings")))
		   (error "assertion failure"))))
	     ((_ exprs others)
	       (or exprs
		 (begin (write-report others
			  ("failed assertion: " 'exprs))
		   (error "assertion failure"))))))
	 )
       (do-assert () _expr . _others)
       ))))


(define-syntax assure
  (syntax-rules ()
    ((assure exp error-msg) (assert exp report: error-msg))))

(define (identify-error msg args . disposition-msgs)
  (let ((port (current-error-port)))
    (newline port)
    (display "ERROR" port)
    (display msg port)
    (for-each (lambda (msg) (display msg port))
	      (append args disposition-msgs))
    (newline port)))

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
    args))

(define (cerr . args)
  (for-each (lambda (x)
              (if (procedure? x) (x (current-error-port))
		(display x (current-error-port))))
    args))

(define nl (string #\newline))

; Some useful increment/decrement (aka predecessor/successor) operators

				; Mutable increment
(define-syntax inc!
  (syntax-rules () ((inc! x) (set! x (+ 1 x)))))

				; Read-only increment
(define-syntax inc
  (syntax-rules () ((inc x) (+ 1 x))))

				; Mutable decrement
(define-syntax dec!
  (syntax-rules () ((dec! x) (set! x (- x 1)))))

				; Read-only decrement
(define-syntax dec
  (syntax-rules () ((dec x) (- x 1))))

; Some useful control operators

			; if condition is true, execute stmts in turn
			; and return the result of the last statement
			; otherwise, return unspecified.
(define-syntax when
  (syntax-rules ()
    ((when condition . stmts)
      (and condition (begin . stmts)))))
  

			; if condition is false execute stmts in turn
			; and return the result of the last statement
			; otherwise, return unspecified.
			; This primitive is often called 'unless'
(define-syntax whennot
  (syntax-rules ()
    ((whennot condition . stmts)
      (or condition (begin . stmts)))))


			; Execute a sequence of forms and return the
			; result of the _first_ one. Like PROG1 in Lisp.
			; Typically used to evaluate one or more forms with
			; side effects and return a value that must be
			; computed before some or all of the side effects happen.
(define-syntax begin0
  (syntax-rules ()
    ((begin0 form form1 ... ) 
      (let ((val form)) form1 ... val))))

			; Prepend an ITEM to a LIST, like a Lisp macro PUSH
			; an ITEM can be an expression, but ls must be a VAR
(define-syntax push!
  (syntax-rules ()
    ((push! item ls)
      (set! ls (cons item ls)))))

			; Is str the empty string?
			; string-null? str -> bool
			; See Olin Shiver's Underground String functions
(define-syntax string-null?
  (syntax-rules ()
    ((string-null? str) (zero? (string-length str)))))


; A rather useful utility from SRFI-1
; cons* elt1 elt2 ... -> object
;    Like LIST, but the last argument provides the tail of the constructed
;    list -- i.e., (cons* a1 a2 ... an) = (cons a1 (cons a2 (cons ... an))).
;
;   (cons* 1 2 3 4) => (1 2 3 . 4)
;   (cons* 1) => 1
(define (cons* first . rest)
  (let recur ((x first) (rest rest))
    (if (pair? rest)
	(cons x (recur (car rest) (cdr rest)))
	x)))

; Look up a value associated with a symbolic key in alist 
; ((key value) ...) or ((key . value) ...)
; and return the associated value.
; If the association has the form
;   (key . value) where value is not a pair --> return value
;   (key   value)                           --> return value
;   (key value1 value2 value3 ...) -> return (value1 value2 value3 ...)
; that is, the procedure tries to do the right thing for
; both kinds of associative lists. 
;
; The form `lookup-def' is a special form rather than a regular
; procedure. Its first two arguments are evaluated exactly once. The
; default-value argument, if given, is evaluated only if the desired key
; is not found. I have not seen any need to pass `lookup-def' as an
; argument to other functions. If the latter is desired, it is not
; difficult to accomplish by explicitly wrapping `lookup-def' into a
; lambda form.
;
; We use a pseudo-keyword argument warn: as a modifier.
; This is not really a keyword argument (although it may be,
; if the Scheme system turns out DSSSL-compatible)
; 
; (lookup-def key alist)  -- lookup the key in the alist and return the
;                        associated value. Raise an error if the key is not
;                        found.
; (lookup-def key alist default-exp)
;                     -- lookup the key in the alist and return the associated
;                        value. If the the key is not found, evaluate
;                        the default-exp and return its result.
; (lookup-def key alist warn: default-exp)
;                     -- the same as above. In addition, write a warning
;                        (using cerr above) if the key is not found.

(define-syntax lookup-def 
  (syntax-rules (warn:)
    ((lookup-def key alist)
      (let ((nkey key) (nalist alist)) ; evaluate them only once
	(let ((res (assq nkey nalist)))
	  (if res
	    (let ((res (cdr res)))
	     (cond
	       ((not (pair? res)) res)
	       ((null? (cdr res)) (car res))
	       (else res)))
	    (error "Failed to find " nkey " in " nalist)))))
    ((lookup-def key alist default-exp)
      (let ((res (assq key alist)))
	(if res
	  (let ((res (cdr res)))
	    (cond
	      ((not (pair? res)) res)
	      ((null? (cdr res)) (car res))
	      (else res)))
	  default-exp)))
    ((lookup-def key alist warn: default-exp)
      (let ((nkey key) (nalist alist)) ; evaluate them only once
	(let ((res (assq nkey nalist)))
	  (if res
	    (let ((res (cdr res)))
	     (cond
	       ((not (pair? res)) res)
	       ((null? (cdr res)) (car res))
	       (else res)))
	    (begin
	      (cerr "Failed to find " nkey " in " nalist #\newline)
	      default-exp)))))
    ))


			; Implementation of SRFI-0
			; Only feature-identifiers srfi-0 and scm
			; assumed predefined. See below why this
			; syntax-rule may NOT use an let-syntax.
;; (define-syntax cond-expand
;;   (syntax-rules (else scm srfi-0 and or not)
;;     ((cond-expand)
;;       (error "Unfulfilled cond-expand"))
;;     ((cond-expand (else . cmd-or-defs*))
;;       (begin . cmd-or-defs*))
;;     ((cond-expand "feature-id" scm kt kf) kt)
;;     ((cond-expand "feature-id" srfi-0 kt kf) kt)
;;     ((cond-expand "feature-id" x kt kf) kf)
;;     ((cond-expand "satisfies?" (and) kt kf) kt)
;;     ((cond-expand "satisfies?" (and clause) kt kf)
;;       (cond-expand "satisfies?" clause kt kf))
;;     ((cond-expand "satisfies?" (and clause . rest) kt kf)
;;       (cond-expand "satisfies?" clause
;; 	(cond-expand "satisfies?" (and . rest) kt kf) kf))
;;     ((cond-expand "satisfies?" (or) kt kf) kf)
;;     ((cond-expand "satisfies?" (or clause) kt kf)
;;       (cond-expand "satisfies?" clause kt kf))
;;     ((cond-expand "satisfies?" (or clause . rest) kt kf)
;;       (cond-expand "satisfies?" clause kt
;; 	(cond-expand "satisfies?" (or . rest) kt kf)))
;;     ((cond-expand "satisfies?" (not clause) kt kf)
;;       (cond-expand "satisfies?" clause kf kt))
;;     ((cond-expand "satisfies?" x kt kf)
;;       (cond-expand "feature-id" x kt kf))

;;     ((cond-expand (feature-req . cmd-or-defs*) . rest-clauses)
;;       (cond-expand "satisfies?" feature-req
;; 	  (begin . cmd-or-defs*)
;; 	  (cond-expand . rest-clauses)))))

; define-opt: A concise definition allowing optional arguments.
; Example:
;
; (define-opt (foo arg1 arg2 (optional arg3 (arg4 init4))) body)
;
; The form define-opt is designed to be as compatible with DSSSL's
; extended define as possible -- while avoiding the non-standard
; lexical token #!optional. On systems that do support DSSSL (e.g.,
; Gambit, Bigloo, Kawa) our define-opt expands into DSSSL's extended
; define, which is implemented efficiently on these systems.
;
; Here's the relevant part of the DSSSL specification, lifted
; from Gambit's online documentation:

;   define-formals = formal-argument-list | r4rs-define-formals
;   formal-argument-list = reqs opts rest keys
;   reqs = required-formal-argument*
;   required-formal-argument = variable
;   opts = #!optional optional-formal-argument* | empty
;   optional-formal-argument = variable | ( variable initializer )
;   rest = #!rest rest-formal-argument | empty
;   rest-formal-argument = variable
;   keys = #!key keyword-formal-argument* | empty
;   keyword-formal-argument = variable | ( variable initializer )
;   initializer = expression
;   r4rs-lambda-formals = ( variable* ) | ( variable+ . variable ) | variable
;   r4rs-define-formals = variable* | variable* . variable
;
;   1. Variables in required-formal-arguments are bound to successive actual
;      arguments starting with the first actual argument. It shall be an error
;      if there are fewer actual arguments than required-formal-arguments.
;   2. Next variables in optional-formal-arguments are bound to remaining
;      actual arguments. If there are fewer remaining actual arguments than
;      optional-formal-arguments, then the variables are bound to the result
;      of evaluating initializer, if one was specified, and otherwise to #f.
;      The initializer is evaluated in an environment in which all previous
;      formal arguments have been bound.
;   It shall be an error for a variable to appear more than once in a
;   formal-argument-list.
;   It is unspecified whether variables receive their value by binding or by
;   assignment.
;
; Our define-opt does not currently support rest and keys arguments.
; Also, instead of #optional optional-formal-argument ...
; we write (optional optional-formal-argument ...)
; 
; Our define-opt is similar to PLT Scheme's opt-lambda. However, 
; the syntax of define-opt guarantees that optional arguments are 
; really at the very end of the arg list.


; SCM does not support DSSSL extended defines and lambdas.
; Caveat: (define-opt name-bindings body) cannot expand into
; (let-syntax ((helper-macro ...)) (helper-macro name-bindings body))
; where helper-macro will generate the valid define.
; The mere appearance of (let-syntax ...) tells the Scheme system
; that whatever define will be generated, it is meant for the _internal_
; context. For example, the following code
;
; (define-syntax tdefine
;   (syntax-rules ()
;     ((tdefine _args . _bodies)
;       (letrec-syntax
; 	((helper
; 	   (syntax-rules ()
; 	     ((helper args bodies) (define args . bodies)))))
; 	(helper _args _bodies)))))
; (tdefine (foo x) (display "OK") (display x) (newline))
; (foo 42)
;
; runs OK on Petite Chez but gives an error "definition in expression context"
; on Scheme48 and SCM (and, consequently, the binding to foo does not occur).


(define-syntax define-opt
  (syntax-rules (optional)
    ((define-opt (name . bindings) . bodies)
      (define-opt "seek-optional" bindings () ((name . bindings) . bodies)))

    ((define-opt "seek-optional" ((optional . _opt-bindings))
       (reqd ...) ((name . _bindings) . _bodies))
      (define (name reqd ... . _rest)
	(letrec-syntax
	  ((handle-opts
	     (syntax-rules ()
	       ((_ rest bodies (var init))
		 (let ((var (if (null? rest) init
			      (if (null? (cdr rest)) (car rest)
				(error "extra rest" rest)))))
		   . bodies))
	       ((_ rest bodies var) (handle-opts rest bodies (var #f)))
	       ((_ rest bodies (var init) . other-vars)
		 (let ((var (if (null? rest) init (car rest)))
		       (new-rest (if (null? rest) '() (cdr rest))))
		   (handle-opts new-rest bodies . other-vars)))
	       ((_ rest bodies var . other-vars)
		 (handle-opts rest bodies (var #f) . other-vars))
	       ((_ rest bodies)		; no optional args, unlikely
		 (let ((_ (or (null? rest) (error "extra rest" rest))))
		   . bodies)))))
	  (handle-opts _rest _bodies . _opt-bindings))))

    ((define-opt "seek-optional" (x . rest) (reqd ...) form)
      (define-opt "seek-optional" rest (reqd ... x) form))

    ((define-opt "seek-optional" not-a-pair reqd form)
      (define . form))			; No optional found, regular define

    ((define-opt name body)		; Just the definition for 'name',
      (define name body))		; for compatibilibility with define
))
