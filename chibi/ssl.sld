
;;> Basic bindings for establishing SSL connections.

(define-library (chibi ssl)
  (export ssl-method ssl-ctx-new ssl-new ssl-ctx-set-mode! ssl-set-fd!
          ssl-do-handshake ssl-shutdown ssl-write ssl-read
          ssl? ssl-method? ssl-context?
          SSLv23_server_method SSLv23_client_method
          SSLv2_server_method SSLv2_client_method
          SSLv3_server_method SSLv3_client_method
          TLSv1_server_method TLSv1_client_method
          ssl-make-i/o-ports)
  (import (scheme base) (srfi 33) (chibi io))
  (include-shared "ssl")
  (include "ssl.scm"))
