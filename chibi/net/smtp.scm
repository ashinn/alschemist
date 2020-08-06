;; smtp.scm -- simple SMTP module
;; Copyright (c) 2005-2015 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Easy mail interface.
;;>
;;> Example:
;;>
;;> \schemeblock{
;;> (send-mail 'From:    "Dr. Watson <guest@grimpen.moor>"
;;>            'To:      "Sherlock Homes <not-really@221B-baker.street>"
;;>            'Subject: "First Report"
;;>            'Charset: "ISO-8859-1"
;;>            'Body:    "Moor is gloomy. Heard strange noise, attached."
;;>            'Attachments: '((File: "howl.ogg")))}
;;>
;;> This tries very hard to do The Right Thing.
;;>
;;> More specifically, the body and headers are all properly MIME-encoded
;;> for the given Charset (which may alternately be specified in
;;> Content-Type), choosing the whichever of base64 and quoted-printable
;;> would encode with the smallest size.  Source strings are all assumed
;;> to be UTF-8 and charset encoded as needed.
;;>
;;> Attachments are arbitrarily nested lists of the same keyword
;;> parameters as in the main send-mail parameters, and are sent as
;;> multipart MIME encodings (i.e. any valid parameter list to send-mail
;;> could also be used in an Attachments: list).  Attachments inherit,
;;> but may override, their parents' Charset settings.
;;>
;;> Most standard mail headers are recognized as keyword arguments,
;;> always titlecased (e.g. Cc:, Message-Id:, Mime-Version:) for a
;;> friendly mail-like interface.  Additional headers may be specified in
;;> the Headers: keyword, for example
;;>
;;>   \scheme{Headers: '(X-Foo: "bar" X-Baz: "qux")}
;;>
;;> The parameters To:, Cc: and Bcc: may also be lists of addresses which
;;> will be properly comma-delimited.
;;>
;;> You can tell send-mail *not* to perform character encoding
;;> conversions by passing the Charconv: #f keyword argument, and
;;> likewise disable automatic MIME encoding of message bodies by passing
;;> Encode: #f, in which case you are responsible for ensuring the
;;> literal text matches with the given Content-type and
;;> Content-Transfer-Encoding.
;;>
;;> If you just want to send a raw message without any processing at all,
;;> you can pass an input-port to the Source: parameter
;;>
;;> \schemeblock{
;;>   (send-mail 'To: addresses 'Source: (open-input-file "message.txt"))}
;;>
;;> By default send-mail connects to the local smtp port, however you can
;;> override this with the Host: parameter to use a particular server.
;;> Alternately, if you specify Host: #f then send-mail will look up the
;;> appropriate DNS entries for all recipients and send to them in turn.
;;>
;;> send-mail returns the list of recipients who couldn't be delivered to
;;> (i.e. a null list when there are no errors).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; types and constants

(define smtp-mime-version "1.0")
(define smtp-default-port 25)
(define smtp-default-tls-port 465)
(define smtp-default-max-col 76)
(define smtp-newline '#u8(13 10))

(define-record-type Smtp
  (%make-smtp sock in out status message debug?)
  smtp?
  (sock smtp-sock set-smtp-sock!)
  (in smtp-in set-smtp-in!)
  (out smtp-out set-smtp-out!)
  (status smtp-status set-smtp-status!)
  (message smtp-message set-smtp-message!)
  (debug? smtp-debug?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

;; char-set conversion currently unsupported
(define (ces-convert x from to)
  (if (string? x)
      (string->utf8 x)
      x))

;; copy data to a port, escaping lines starting with . as needed
(define (copy-data-port in out)
  (let lp ((newline? #t))
    (let ((i (read-u8 in)))
      (cond
       ((eof-object? i))
       (else
        (write-u8 i out)
        (if (and newline? (eqv? i (char->integer #\.)))
            (write-u8 i out))
        (lp (eqv? i (char->integer #\newline))))))))

(define (call-with-output-bytevector proc)
  (let ((out (open-output-bytevector)))
    (proc out)
    (get-output-bytevector out)))

(define (string-chop str len)
  (let ((limit (string-length str)))
    #f))

(define (smtp-write-data x out)
  (copy-data-port (open-input-bytevector (if (string? x) (string->utf8 x) x))
                  out))

;; this is just used for unique boundaries
(define (current-seconds-string) ; trim the .0
  (let* ((now (number->string (round (current-seconds))))
         (now-len (string-length now)))
    (if (eqv? #\. (string-ref now (- now-len 2)))
        (substring now 0 (- now-len 2))
        now)))

;; RFC 2822 format: Tue, 16 Dec 2008 11:45:13 +0000
(define current-rfc-2822-date-string
  (let ((pad (lambda (n)
               (string-append (if (< n 10) "0" "") (number->string n))))
        (days '#("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
        (months '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
    (lambda ()
      (let* ((t (seconds->time (current-seconds)))
             (tz (time-offset t)))
        (string-append
         (vector-ref days (time-day-of-week t)) ", " (pad (time-day t)) " "
         (vector-ref months (time-month t)) " "
         (pad (+ 1900 (time-year t)))
         " " (pad (time-hour t)) ":" (pad (time-minute t))
         ":" (pad (time-second t)) " " (if (< tz 0) "-" "+")
         (pad (quotient (abs tz) (* 60 60)))
         (pad (remainder (quotient (* 100 (abs tz)) (* 60 60)) 100)))))))

(define (get-user-name)
  (user-name (user-information (current-user-id))))

(define (domain-part addr)
  (let lp ((i (- (string-length addr) 1)))
    (and (>= i 0)
         (if (eqv? #\@ (string-ref addr i))
             (substring addr (+ i 1))
             (lp (- i 1))))))

(define (local-part addr)
  (let lp ((i (- (string-length addr) 1)))
    (if (< i 0)
        addr
        (if (eqv? #\@ (string-ref addr i))
            (substring addr 0 i)
            (lp (- i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns (address name)
(define parse-mail-address
  (let* ((name-pat
          '(-> name
               (~ space ("\\\""))
               (* (~ ("<\\\"")))
               (~ space ("\\\""))))
         (addr-pat
          '(-> addr
               (+ (~ space ("\\\"@")))
               "@"
               (+ (~ space ("\\\"@.")))
               (* "." (+ (~ space ("\\\"@."))))))
         (pat
          (regexp
           `(: (* space)
               (or (: ,name-pat (* space) "<" ,addr-pat ">")
                   (: "\"" (-> name (* (~ ("\\\"")))) "\"" (* space)
                      "<" ,addr-pat ">")
                   (: ,addr-pat (* space) "(" ,name-pat ")")
                   ,addr-pat)
               (* space)))))
    (lambda (addr)
      (and (string? addr)
           (cond
            ((regexp-matches pat addr)
             => (lambda (md)
                  (list (regexp-match-submatch md 'addr)
                        (regexp-match-submatch md 'name))))
            (else #f))))))

(define (parse-mail-address-list str)
  (map parse-mail-address (string-split str ",")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing smtp responses

(define (smtp-get-response smtp)
  (let lp ((line (read-line (smtp-in smtp))) (msg '()))
    (cond
     ((eof-object? line)
      (smtp-close smtp)
      #f)
     (else
      (cond
       ((smtp-debug? smtp)
        (write-string "smtp-resp: " (current-error-port))
        (write-string line (current-error-port))
        (newline (current-error-port))))
      (let ((len (string-length line)))
        (if (and (>= len 4) (eqv? #\- (string-ref line 3)))
            (lp (read-line (smtp-in smtp)) (cons (substring line 4) msg))
            (and-let* (((>= len 3))
                       (status (string->number (substring line 0 3)))
                       (str (if (= len 3) "" (substring line 4))))
              (set-smtp-status! smtp status)
              (set-smtp-message! smtp (string-join (cons str msg) " "))
              (< status 400))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connecting

(define (smtp-open host . o)
  (let-optionals* o ((port #f)
                     (from-host (get-host-name))
                     (debug? #f))
    (let ((x (guard (exn (else #f))
               (open-net-io host (or port smtp-default-port)))))
      (and x
           (let* ((sock (first x))
                  (in (second x))
                  (out (third x))
                  (smtp (%make-smtp sock in out 200 "" debug?)))
             (if (and (smtp-get-response smtp)
                      (smtp-helo smtp from-host))
                 smtp
                 (begin
                   (smtp-close smtp)
                   #f)))))))

(define (smtp-close smtp)
  (close-input-port (smtp-in smtp))
  (close-output-port (smtp-out smtp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic commands

;; catch i/o errors
(define (make-smtp-command name)
  (let ((sname (if (symbol? name) (symbol->string name) name)))
    (lambda (smtp . o)
      (let ((out (smtp-out smtp)))
        (guard (exn (else
                     (set-smtp-status! smtp 554) ; transaction failed
                     (set-smtp-message! smtp #f)
                     #f))
          (cond
           ((smtp-debug? smtp)
            (write-string "smtp-send: " (current-error-port))
            (write-string sname (current-error-port))
            (cond
             ((pair? o)
              (write-u8 (char->integer #\space) (current-error-port))
              (write-string (car o) (current-error-port))))
            (newline (current-error-port))))
          (write-string sname out)
          (cond
           ((pair? o)
            (write-u8 (char->integer #\space) out)
            (write-string (car o) out)))
          (write-bytevector smtp-newline out)
          (flush-output-port out)
          (smtp-get-response smtp))))))

(define smtp-helo      (make-smtp-command 'HELO))
(define smtp-mail      (make-smtp-command 'MAIL))
(define smtp-rcpt      (make-smtp-command 'RCPT))
(define smtp-data      (make-smtp-command 'DATA))
(define smtp-reset     (make-smtp-command 'RSET))
(define smtp-verify    (make-smtp-command 'VRFY))
(define smtp-expand    (make-smtp-command 'EXPN))
(define smtp-help      (make-smtp-command 'HELP))
(define smtp-quit      (make-smtp-command 'QUIT))
(define smtp-noop      (make-smtp-command 'NOOP))
(define %smtp-starttls (make-smtp-command 'STARTTLS))

;; STARTTLS is a hack.  Instead of just wrapping the entire
;; protocol in a clean layer like normal SSL, you start out in an
;; unencrypted protocol and then request to switch to SSL,
;; complicating every protocol that uses this and breaking the nice
;; `ssl-connect' abstraction by forcing us to put protocol-specific
;; code here.
(define (smtp-starttls smtp)
  (and (%smtp-starttls smtp)
       (and-let* ((ctx (ssl-ctx-new (ssl-method 'tls #f)))
                  (x (ssl-make-i/o-ports (ssl-new ctx)
                                         (smtp-sock smtp)
                                         (smtp-in smtp)
                                         (smtp-out smtp))))
         (set-smtp-in! smtp (car x))
         (set-smtp-out! smtp (cadr x))
         #t)))

(define (smtp-mail-from smtp address)
  (write-bytevector (string->utf8 "MAIL FROM:<") (smtp-out smtp))
  (write-bytevector (string->utf8 address) (smtp-out smtp))
  (write-bytevector (string->utf8 ">\r\n") (smtp-out smtp))
  (flush-output-port (smtp-out smtp))
  (smtp-get-response smtp))

(define (smtp-recipient smtp address)
  (write-bytevector (string->utf8 "RCPT TO:<") (smtp-out smtp))
  (write-bytevector (string->utf8 address) (smtp-out smtp))
  (write-bytevector (string->utf8 ">\r\n") (smtp-out smtp))
  (flush-output-port (smtp-out smtp))
  (smtp-get-response smtp))

(define count
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      n)))

(define (smtp-generate-boundary)
  (string-append "------Multipart_" (number->string (current-process-id))
                 "_g" (number->string (count))
                 "." (current-seconds-string)))

(define (smtp-generate-msgid)
  (string-append "<" (current-seconds-string)
                 "." (number->string (current-process-id))
                 ".g" (number->string (count))
                 "@" (get-host-name) ">"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-level interface

(define (build-mx-table rcpt-ls localhost . o)
  (let ((domain-mxs (if (pair? o) (car o) (make-hash-table string=?)))
        (hosts (make-hash-table string=?)))
    (for-each
     (lambda (addr)
       (let* ((domain (or (domain-part addr) localhost))
              (mx-ls (or (hash-table-ref/default domain-mxs domain #f)
                         (let ((res (dns-mx domain)))
                           (hash-table-set! domain-mxs domain res)
                           res)))
              (mx (and (pair? mx-ls) (cadr (car mx-ls)))))
         (if (string? mx)
             (let ((addrs (cons addr (hash-table-ref/default hosts mx '()))))
               (hash-table-set! hosts mx addrs)))))
     rcpt-ls)
    hosts))

;; convenience routine to send a single message
(define (send-mail . args)
  (let-keywords* args
      ((Host "localhost")
       (User (get-user-name))
       (Password #f)
       (Localhost (get-host-name))
       (From (string-append User "@" Localhost))
       (From-Base (car (parse-mail-address From)))
       (From-Addr From-Base)
       (To '())
       (Cc '())
       (Bcc '())
       (Source #f)
       (Debug #f)
       (StartTLS? #f)
       (Port (if StartTLS? smtp-default-tls-port smtp-default-port))
       (Auth-Method #f)
       (No-Sendmail #f)
       keys)
    (let* ((To (if (list? To) To (list To)))
           (Cc (if (list? Cc) Cc (list Cc)))
           (Bcc (if (list? Bcc) Bcc (list Bcc)))
           (Recipients (map car (map parse-mail-address (append To Cc Bcc))))
           (keys `(From: ,From
                         ,@(if (pair? To) `(To: ,To) '())
                         ,@(if (pair? Cc) `(Cc: ,Cc) '())
                         ,@keys)))
      (define (send-output out)
        (cond
         ((input-port? Source) (copy-data-port Source out))
         ((procedure? Source)  (Source out))
         ((string? Source)     (smtp-write-data Source out))
         (else (apply smtp-write-message out keys)))
        (write-string "\r\n.\r\n" out)
        (flush-output-port out))
      (define (send host rcpt in)
        (and-let* ((smtp (smtp-open host Port Localhost Debug)))
          ;; optionally negotiate TLS and authenticate
          (if StartTLS?
              (smtp-starttls smtp))
          (if (or Password Auth-Method)
              #f)
          ;; mail to all recipients
          (let ((res 
                 (and (smtp-mail-from smtp From-Addr)
                      (every (lambda (r) (smtp-recipient smtp r)) rcpt)
                      (smtp-data smtp)
                      (begin
                        (send-output (smtp-out smtp))
                        (smtp-get-response smtp)))))
            (smtp-quit smtp)
            (smtp-close smtp)
            res)))
      (if (string? Host)
          ;; local or smarthost
          (if (send Host Recipients Source)
              '()
              ;; if relaying to the localhost failed, optionally fall back
              ;; to the sendmail executable
              (if (and (string-ci=? Host "localhost") (not No-Sendmail))
                  (call-with-process-io
                   `("sendmail" ,@Recipients)
                   (lambda (pid proc-in proc-out proc-err)
                     (send-output proc-in)
                     (close-output-port proc-in)
                     (let ((ls (waitpid pid 0)))
                       (if (and (pair? ls) (zero? (cadr ls)))
                           '()
                           Recipients))))
                  Recipients))
          ;; buffer message to send to multiple smtp hosts
          (let ((source (or Source (call-with-output-bytevector send-output)))
                (hosts (build-mx-table Recipients Localhost)))
            ;; connect to each host and deliver the message in turn to all
            ;; recipients on that host
            (let ((failures '()))   ; XXXX TODO, try secondary MX hosts
              (hash-table-walk
               hosts
               (lambda (host ls)
                 (unless (send host ls source)
                   (set! failures (append ls failures)))))
              failures))))))

(define (smtp-write-message port . keys)
  (apply smtp-write-headers port keys)
  (apply smtp-write-mime-part port keys))

(define (multi-part-type? str)
  (and str (string-contains (string-downcase-ascii str) "multipart/") #t))

(define (ascii-string? str)
  (let lp ((i (- (string-length str) 1)))
    (or (< i 0)
        (and (< (char->integer (string-ref str i)) 127)
             (lp (- i 1))))))

(define (mime-choose-encoding str)
  (let lp ((i (- (string-length str) 1)) (ascii 0) (qp 0) (hi 0))
    (if (< i 0)
        (cond
         ((zero? hi) "7-bit")
         ((<= (+ ascii (* 3 (+ qp hi)))
              (/ (* 4 (+ ascii qp hi)) 3))
          "quoted-printable")
         (else "base64"))
        (let ((c (char->integer (string-ref str i))))
          (cond
           ((>= c 128)
            (lp (- i 1) ascii qp (+ hi 1)))
           ((and (<= 33 c 126) (not (memq c '(61 63 95))))
            (lp (- i 1) (+ ascii 1) qp hi))
           (else
            (lp (- i 1) (+ ascii 1) (+ qp 1) hi)))))))

(define (mime-choose-encoder str)
  (let ((enc (mime-choose-encoding str)))
    (cond
     ((equal? "quoted-printable" enc) quoted-printable-encode-header)
     ((equal? "base64" enc) base64-encode-header)
     (else (lambda (x) x)))))

(define (mime-wrap-header-value val . o) ; try to wrap after semi-colons
  (let* ((width (if (pair? o) (car o) 0))
         (nl (if (and (pair? o) (pair? (cdr o))) (cadr o) "\r\n"))
         (sep (string-append nl "\t")))
    (let lp ((i (string-cursor-start val))
             (max-len (- smtp-default-max-col width))
             (len (string-cursor-end val))
             (res '()))
      (cond
       ((< (- len i) max-len)
        (string-join (reverse (cons (substring-cursor val i) res)) sep))
       (else
        (let ((i2 (string-find val ";" i len)))
          (cond
           ((< i2 len)
            (lp (+ i2 1)
                smtp-default-max-col
                (- len (- (+ i2 1) i))
                (cons (mime-wrap-header-value
                       (substring-cursor val i (+ i2 1))
                       width
                       nl)
                      res)))
           (else
            (let ((i2 (+ i max-len)))
              (lp i2 smtp-default-max-col (- len max-len)
                  (cons (substring-cursor val i i2) res)))))))))))

(define (mime-encode-ascii-header name val . o)
  (let* ((nl (if (pair? o) (car o) "\r\n"))
         (name (if (symbol? name) (symbol->string name) name))
         (len1 (+ 2 (string-length name)))
         (len2 (string-length val)))
    (cond
     ((<= (+ len1 len2) smtp-default-max-col)
      (string-append name " " val))
     ((>= len1 smtp-default-max-col) ; the header name itself is too long
      (string-append name "" nl "\t" (mime-wrap-header-value val 0 nl)))
     (else
      (string-append name " " (mime-wrap-header-value val len1 nl))))))

(define (mime-encode-header name val charset . o)
  (let* ((nl (if (pair? o) (car o) "\r\n"))
         (name (if (symbol? name) (symbol->string name) name))
         (name (if (string-suffix? ":" name) name (string-append name ":"))))
    (if (ascii-string? val)
        (mime-encode-ascii-header name val nl)
        (let* ((str (if charset (ces-convert val "UTF-8" charset) val))
               (encode (mime-choose-encoder str)))
          (string-append name " " (encode (or charset "UTF-8") val
                                          (+ 1 (string-length name))
                                          smtp-default-max-col nl))))))

(define *default-smtp-headers*
  `(Return-Path: Envelope-To: Delivery-Date: Received:
    (Message-Id: ,smtp-generate-msgid)
    (From: ,(lambda () (string-append (get-user-name) "@" (get-host-name))))
    Reply-To: To: Cc: (Subject: ,(lambda () ""))
    (Date: ,current-rfc-2822-date-string)
    X-Mailer: X-Accept-Language: User-Agent: Organization: X-Face:
    (Mime-Version: ,(lambda () smtp-mime-version)) X-Loop: X-Priority:
    ))

(define (smtp-write-headers port . args)
  (let-keywords* args
      ((Content-Type #f)
       (Charset (and Content-Type
                     (assq-ref (mime-parse-content-type Content-Type)
                               'charset)))
       (Charconv #t)
       (Headers '())
       (Newline "\r\n")
       keys)
    (let ((cset (and Charconv Charset)))
      (for-each ;; standard headers
       (lambda (x)
         (let* ((name (if (pair? x) (car x) x))
                (val1 (keyword-ref* keys name (and (pair? x) ((cadr x)))))
                (val (cond ((pair? val1) (string-join val1 ", "))
                           ((null? val1) #f)
                           (else val1))))
           (cond
            (val
             (write-string (mime-encode-header name val cset Newline) port)
             (write-string Newline port)))))
       *default-smtp-headers*)
      (let lp ((ls Headers)) ;; custom headers
        (when (pair? ls)
          (write-string (mime-encode-header (car ls) (cadr ls) cset Newline)
                        port)
          (write-string Newline port)
          (lp (cddr ls)))))))

;; recursively write mime-parts
(define (smtp-write-mime-part port . args)
  (let-keywords* args
      ((File #f)
       (Body #f)
       (Content-Type #f)
       (Boundary (smtp-generate-boundary))
       (Charset #f)
       (Source-Charset "UTF-8")
       (Charconv #t)
       (Content-Transfer-Encoding #f)
       (Content-Disposition #f)
       (Encode #t)
       (Attachments '())
       (Newline "\r\n"))
    (let* ((ctype (if (pair? Attachments)
                      (if (multi-part-type? Content-Type)
                          Content-Type
                          (string-append "multipart/mixed; boundary=\""
                                         Boundary "\""))
                      (or Content-Type "text/plain")))
           (ctype-ls (mime-parse-content-type ctype))
           (cset (and Charconv
                      (or Charset (assq-ref (cdr ctype-ls) 'charset))))
           (str (or (and Body
                         (if (and Charconv cset (not (ascii-string? Body)))
                             (ces-convert Body Source-Charset cset)
                             Body))
                    (and File (with-input-from-file File read-string))
                    ""))
           (cenc (and Encode (or Content-Transfer-Encoding
                                 (and (not (ascii-string? str))
                                      (mime-choose-encoding str)))))
           (ctype2 (if (and Charset (not File) (not (pair? Attachments))
                            (not (string-contains (string-downcase-ascii ctype)
                                                  "charset=")))
                       (string-append ctype "; charset=\"" Charset "\"")
                       ctype))
           (ctype3 (if (and File (not (pair? Attachments))
                            (not (string-contains (string-downcase-ascii ctype)
                                                  "filename=")))
                       (string-append ctype2 "; filename=\""
                                      (path-strip-directory File)
                                      "\"")
                       ctype2)))
      ;; write the type and optionally transfer-encoding headers
      (write-string (mime-encode-header "Content-Type" ctype3 cset) port)
      (write-string Newline port)
      (cond
       ((or Content-Transfer-Encoding cenc)
        (write-string (mime-encode-header "Content-Transfer-Encoding"
                                     (or Content-Transfer-Encoding cenc)
                                     cset)
                 port)
        (write-string Newline port)))
      (cond
       (Content-Disposition
        (write-string (mime-encode-header "Content-Disposition"
                                     Content-Disposition
                                     cset)
                 port)
        (write-string Newline port)))
      (write-string Newline port)
      ;; write the attachments
      (cond
       ((pair? Attachments)
        (let ((Attachments
               (cons (list
                      'Body: Body
                      'File: File
                      'Content-Type: Content-Type
                      'Content-Transfer-Encoding: Content-Transfer-Encoding)
                     Attachments)))
          (for-each
           (lambda (keys)
             (write-string "--" port)
             (write-string Boundary port)
             (write-string Newline port)
             (write-string Newline port)
             (apply smtp-write-mime-part port
                    (append keys
                            (list 'Charset: Charset 'Charconv: Charconv)))
             (write-string Newline port))
           Attachments)
          (write-string "--" port)
          (write-string Boundary port)
          (write-string Newline port)))
       ;; no attachments, no body
       ((member str '("" #u8())))
       ;; single body
       (else
        (cond
         ((equal? cenc "quoted-printable")
          (write-bytevector
           (quoted-printable-encode-bytevector (string->utf8 str))
           port))
         ((equal? cenc "base64")  ; XXXX slow
          (write-string
           (string-join
            (string-chop (base64-encode-bytevector (string->utf8 str))
                         smtp-default-max-col)
            Newline)
           port))
         (else (smtp-write-data str port)))
        (write-bytevector smtp-newline port))))))
