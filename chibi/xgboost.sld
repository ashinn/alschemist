
(define-library (chibi xgboost)
  (import (scheme base) (scheme write) (chibi string))
  (export-all)
  (include-shared "xgboost/xgboost")
  (include "xgboost/interface.scm"))
