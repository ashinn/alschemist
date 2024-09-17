
(define-library (chibi math autodiff)
  (import (scheme base) (scheme write)
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
   .expt .exp .log .tanh .rectify .sum .dot .sum-axis .sum-axis/squeeze
   .square .mean
   )
  (include "autodiff.scm"))
