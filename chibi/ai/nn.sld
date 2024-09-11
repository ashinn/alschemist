
(define-library (chibi ai nn)
  (import (scheme base)
          (srfi 27)
          (srfi 231)
          (chibi math autodiff)
          (chibi math linalg)
          (chibi math stats))
  (export gradient-descent rms-gradient-descent adam-gradient-descent
          naked-gradient-descent velocity-gradient-descent
          l2-loss sampling-loss smooth
          learning-rate max-learning-iterations learning-batch-size
          loss-epsilon velocity-retained gradient-decay-rate
          )
  (include "nn.scm"))
