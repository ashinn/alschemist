
(define-library (chibi math autodiff)
  (import (scheme base)
          (scheme inexact)
          (scheme hash-table)
          (scheme list)
          (srfi 231)
          (chibi assert)
          (chibi math linalg))
  (export
   let-duals let*-duals
   gradient const dual dual? dual-value dual-link
   .+ .* .- ./ .@ .expt .exp .log .tanh .relu .sum .sum-axis .mean
   )
  (include "autodiff.scm"))
