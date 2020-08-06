(define-library (chibi net smtp)
  (export
   send-mail mime-encode-header mime-choose-encoding
   smtp-write-message smtp-write-headers smtp-write-mime-part
   smtp-generate-msgid smtp-generate-boundary
   smtp-open smtp-close smtp-quit smtp-helo smtp-starttls
   smtp-mail smtp-rcpt smtp-data smtp-noop smtp-reset
   smtp-verify smtp-expand smtp-mail-from smtp-recipient
   smtp-status smtp-message smtp-help smtp?
   domain-part local-part current-rfc-2822-date-string
   parse-mail-address parse-mail-address-list)
  (import (scheme base) (scheme file) (scheme char)
          (srfi 1) (srfi 2) (srfi 69)
          (chibi net) (chibi net dns) (chibi optional) (chibi string)
          (chibi regexp) (chibi system) (chibi process) (chibi pathname)
          (chibi time) (chibi mime) (chibi base64) (chibi quoted-printable))
  (cond-expand
   ((library (chibi ssl))
    (import (chibi ssl)))
   (else
    (begin
      (define (ssl-ctx-new . o) #f)
      (define (ssl-new . o) #f)
      (define (ssl-method . o) #f)
      (define (ssl-make-i/o-ports . o) #f))))
  (include "smtp.scm"))
