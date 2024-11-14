
(define-library (chibi table)
  (import (scheme base)
          (scheme file)
          (scheme list)
          (scheme vector)
          (srfi 227)
          (srfi 231)
          (chibi assert)
          (chibi csv)
          (chibi optional)
          (chibi math linalg))
  (export table table? table-copy table-ref table-set!
          table-num-rows table-num-columns table-shape
          table-arrays table-type-specs table-labels
          table->array table-column table-column-storage
          table-column-define! table-column-set!
          table-column-adjoin! table-column-adjoin
          table-column-drop! table-column-drop
          table-consolidate! table-consolidate
          table-load-csv
          infer-csv-grammar infer-csv-types)
  (include "table.scm"))
