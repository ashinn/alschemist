
(define-library (chibi show python)
  (export
   py-in-expr py-in-stmt
   py-paren py-maybe-paren py-literal? py-literal char->py-char
   py-expr py-expr/sexp py-apply py-op py-indent py-indent-string
   py-slice
   py-wrap-stmt
   py-block py-begin py-try
   py-def py-class py-enum
   py-while py-for py-if
   py-break py-continue py-return py-yield
   py+ py- py* py/ py% py& py^ py~ py! py: py&& py<< py>> py== py!=
   py< py> py<= py>= py= py+= py-= py*= py/= py%= py&= py^= py<<= py>>=
   py-bit-or py-or py-bit-or=
   py-comment py-array
   tensor-dialect)
  (import (scheme base)
          (scheme cxr)
          (scheme list)
          (scheme write)
          (srfi 130)
          (srfi 166)
          (srfi 231)
          (chibi math linalg))
  (include "python.scm"))
