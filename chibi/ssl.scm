
;;> A utility to generate the appropriate method for the \var{protocol},
;;> as a client unless the optional \var{server?} is true.  \var{Protocol}
;;> should be one of the symbols 'sslv2-or-v3, 'sslv2, 'sslv3 or 'tls.

(define (ssl-method protocol . o)
  (let ((server? (and (pair? o) (car o))))
    (case protocol
      ((sslv2-or-v3) (if server? (SSLv23_server_method) (SSLv23_client_method)))
      ((sslv2) (if server? (SSLv2_server_method) (SSLv2_client_method)))
      ((sslv3) (if server? (SSLv3_server_method) (SSLv3_client_method)))
      ((tls) (if server? (TLSv1_server_method) (TLSv1_client_method)))
      (else (error "invalid SSL/TLS connection protocol")))))

;;> A wrapper around SSL_CTX_new for the given \var{protocol}, as symbol
;;> as in \scheme{ssl-method}, setting SSL_MODE_ENABLE_PARTIAL_WRITE
;;> and SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER by default.

(define (ssl-ctx-new protocol . o)
  (let* ((server? (and (pair? o) (car o)))
         (method (ssl-method protocol server?))
         (ctx (%ssl-ctx-new method)))
    (ssl-ctx-set-mode! ctx (bitwise-ior SSL_MODE_ENABLE_PARTIAL_WRITE
                                        SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER))
    ctx))

;;> Wrapper around SSL_new.  Creates a new SSL structure which is
;;> needed to hold the data for a TLS/SSL connection.  Takes settings
;;> from an ssl-context, which can either be provided directly as
;;> \var{x}, or created implicitly using the same signature as
;;> \scheme{ssl-ctx-new}.

(define (ssl-new x . o)
  (SSL_new (if (ssl-context? x) x (apply ssl-ctx-new x o))))

;;> Given the ssl context \var{ssl}, as created with \scheme{ssl-new},
;;> establishes an SSL connection on ports \var{in} and \var{out} from
;;> file descriptor \var{fd} and returns a list of two new ports, the
;;> wrapped input and output of the SSL connection.

(define (ssl-make-i/o-ports ssl fd in out)
  (ssl-set-fd! ssl fd)
  (ssl-do-handshake ssl)
  (list (make-custom-binary-input-port
         (lambda (bv start end)
           (if (zero? start)
               (ssl-read ssl bv end)
               (let* ((tmp (make-bytevector (- end start)))
                      (n (ssl-read ssl tmp (- end start))))
                 (bytevector-copy! bv start tmp)
                 n))))
        (make-custom-binary-output-port
         (lambda (bv start end)
           (if (zero? start)
               (ssl-write ssl bv end)
               (ssl-write ssl (bytevector-copy bv start end) (- end start)))))))
