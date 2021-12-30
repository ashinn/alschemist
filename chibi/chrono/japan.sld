
(define-library (chibi chrono japan)
  (import (scheme base) (chibi locale)
          (chibi chrono base) (chibi chrono common))
  (export chronology:japan make-japanese-time)
  (include "japan.scm"))
