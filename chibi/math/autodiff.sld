
(define-library (chibi math autodiff)
  (import (scheme base)
          (scheme inexact)
          (scheme hash-table)
          (scheme list)
          (srfi 231)
          (chibi assert)
          (chibi math linalg))
  (export
   let-duals let*-duals zero one
   gradient const dual dual? dual-value dual-link as-dual
   .+ .* .- ./ .@
   .expt .exp .log .tanh .relu .sum .dot .sum-axis .square .mean
   )
  (include "autodiff.scm"))
