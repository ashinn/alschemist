
(define-library (chibi math autodiff)
  (import (scheme base) (scheme write)
          (scheme inexact)
          (scheme hash-table)
          (scheme lazy)
          (scheme list)
          (srfi 231)
          (chibi assert)
          (chibi math linalg))
  (export
   let-duals let*-duals zero one
   gradient const dual dual? dual-value dual-link as-dual
   dual-label dual-with-label
   .+ .* .- ./ .@
   .expt .exp .log .tanh .rectify .sum .dot .sum-axis .sum-axis/squeeze
   .square .mean .reshape .transpose
   )
  (begin
    (define (pp x)
      (cond ((dual? x) (pp (dual-value x)))
            ((array? x) (array->list* x))
            (else x))))
  (cond-expand
   ((or debug autodiff-debug autodiff-trace)
    (begin
      (define-syntax dual-op-label
        (syntax-rules ()
          ((dual-op-label label arg ...)
           (list label arg ...))))))
   (else
    (begin
      (define-syntax dual-op-label
        (syntax-rules ()
          ((dual-op-label label arg ...)
           label))))))
  (cond-expand
   ((or debug autodiff-debug)
    ;; or just use (chibi log)?
    (begin
      (define-syntax log-debug
       (syntax-rules ()
         ((log-debug arg0 arg ...)
          (begin
            (write (pp arg0))
            (begin (write-string " ") (write (pp arg))) ...
            (newline)))))))
   (else
    (begin
      (define-syntax log-debug
        (syntax-rules ()
          ((log-debug args ...) #f))))))
   (include "autodiff.scm"))
