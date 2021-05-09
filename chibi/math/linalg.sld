
(define-library (chibi math linalg)
  (import (scheme base) (scheme list) (scheme write)
          (srfi 33) (srfi 179)
          (chibi assert) (chibi optional))
  (export array= array-concatenate identity-array
          array-inverse determinant
          array-mul array-expt array-div-left array-div-right
          array-add-elements! array-sub-elements!
          array-mul-elements! array-div-elements!
          pretty-print-array)
  (include "linalg.scm"))
