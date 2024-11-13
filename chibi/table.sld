
(define-library (chibi table)
  (import (scheme base)
          (scheme file)
          (scheme list)
          (scheme vector)
          (srfi 227)
          (srfi 231)
          (chibi csv)
          (chibi optional)
          (chibi math linalg))
  (export table table? table-ref table-set!
          table-num-rows table-num-columns table-shape
          table-arrays table-type-specs table-labels
          table->array table-column table-column-storage
          table-load-csv
          infer-csv-grammar infer-csv-types)
  (include "table.scm"))
