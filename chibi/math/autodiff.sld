
(define-library (chibi math autodiff)
  (import (scheme base)
          (scheme write)
          (scheme inexact)
          (scheme hash-table)
          (scheme lazy)
          (scheme list)
          (srfi 231)
          (chibi assert)
          (chibi math linalg)
          (chibi math dual)
          (chibi math debug))
  (export
   let-duals let*-duals zero one
   gradient const dual dual? dual-value dual-label dual-link as-dual
   dual-label dual-with-label
   .+ .* .- ./ .@
   .expt .exp .log .tanh .rectify .sum .dot .sum-axis .sum-axis/squeeze
   .square .mean .reshape .transpose
   )
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
   (include "autodiff.scm"))
