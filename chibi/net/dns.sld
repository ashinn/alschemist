(define-library (chibi net dns)
  (export dns-lookup dns-text dns-mx
          dns-build-query dns-build-response
          dns/QUERY dns/IQUERY dns/STATUS dns/A dns/IN dns/TXT dns/MX)
  (import (scheme base) (scheme file) (scheme write)
          (srfi 26) (srfi 27) (srfi 33) (srfi 95) (srfi 130)
          (chibi optional) (chibi net))
  (include "dns.scm"))
