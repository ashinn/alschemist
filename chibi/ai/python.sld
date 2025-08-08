
(define-library (chibi ai python)
  (import (scheme base)
          (scheme list)
          (scheme read)
          (scheme write)
          (srfi 130)
          (srfi 166)
          (srfi 227)
          (srfi 231)
          (chibi ai nn)
          (chibi assert)
          (chibi math debug)
          (chibi math linalg)
          (only (chibi optional) let-keywords*)
          (chibi shell)
          (chibi show python))
  (export network->torch network->torch-output block->torch)
  (cond-expand
   ((library (srfi 98))
    (import (srfi 98))
    (begin
      (define default-python-binary
        (or (get-environment-variable "CHIBI_PYTHON_BINARY")
            "python3"))))
   (else
    (begin
      (define default-python-binary "python3"))))
  (include "python.scm"))
