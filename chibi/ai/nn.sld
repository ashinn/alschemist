
(define-library (chibi ai nn)
  (import (scheme base) (scheme write) (chibi ast)
          (scheme list)
          (srfi 27)
          (srfi 231)
          (chibi math autodiff)
          (chibi math linalg)
          (chibi math stats))
  (export linear relu block block? block-fn block-ls grid-search
          accuracy class= init-weights
          block-compose stack-blocks dense-block model linear relu
          gradient-descent rms-gradient-descent adam-gradient-descent
          naked-gradient-descent velocity-gradient-descent
          l2-loss sampling-loss smooth
          learning-rate max-learning-iterations learning-batch-size
          loss-epsilon velocity-retained gradient-decay-rate
          )
  (include "nn.scm"))
