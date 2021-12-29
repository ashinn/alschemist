
(define-library (chibi chrono tzdb)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme list)
          (srfi 130))
  (export tzdb-parse tzdb-parse-file tzdb-parse-zones tzdb-parse-zone-file)
  (include "tzdb.scm"))
