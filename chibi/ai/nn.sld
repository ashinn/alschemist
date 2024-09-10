
(define-library (chibi ai nn)
  (import (scheme base) (scheme write)
          (srfi 231)
          (chibi math autodiff)
          (chibi math linalg)
          (chibi math stats))
  (export gradient-descent
          naked-gradient-descent velocity-gradient-descent
          l2-loss
          learning-rate max-learning-iterations learning-batch-size
          loss-epsilon velocity-retained
          )
  (include "nn.scm"))
