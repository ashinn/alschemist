;; dns.scm -- Domain Name Service library
;; Copyright (c) 2005-2015 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;> Domain Name Service library, with high-level utilities for
;;> address, mx and text record lookups.  See
;;> http://tools.ietf.org/html/rfc1035.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bytevector utilities

(define (bytevector-append-map ls proc)
  (if (pair? ls)
      (if (pair? (cdr ls))
          (apply bytevector-append (map proc ls))
          (proc (car ls)))
      '#u8()))

(define u16vector vector)
(define u16vector-length vector-length)
(define u16vector-ref vector-ref)

(define (u16vector->bytevector/be u16)
  (let* ((len (u16vector-length u16))
         (res (make-bytevector (* len 2))))
    (do ((i 0 (+ i 1)))
        ((= i len) res)
      (let ((v (u16vector-ref u16 i)))
        (bytevector-u8-set! res (* i 2) (arithmetic-shift v -8))
        (bytevector-u8-set! res (+ (* i 2) 1) (bitwise-and v #xFF))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

;; types
(define dns/A 1)
(define dns/NS 2)
(define dns/MD 3)
(define dns/MF 4)
(define dns/CNAME 5)
(define dns/SOA 6)
(define dns/MB 7)
(define dns/MG 8)
(define dns/MR 9)
(define dns/NULL 10)
(define dns/WKS 11)
(define dns/PTR 12)
(define dns/HINFO 13)
(define dns/MINFO 14)
(define dns/MX 15)
(define dns/TXT 16)

;; qtypes
(define dns/AXFR 252)
(define dns/MAILB 253)
(define dns/MAILA 254)
(define dns/* 255)

;; classes
(define dns/IN 1)
(define dns/CS 2)
(define dns/CH 3)
(define dns/HS 4)

;; opcodes
(define dns/QUERY 0)
(define dns/IQUERY 1)
(define dns/STATUS 2)

(define dns-max-id (- (arithmetic-shift 1 16) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; low-level interface

(define (bool->int n x) (if n (arithmetic-shift 1 x) 0))

(define (dns-header id query? opcode authoritative? truncated?
                    recursion-desired? recursion-available?
                    response-code question-count answer-count
                    authority-count additional-count)
  (let ((flags
         (bitwise-ior (bool->int (not query?) 15)
                      (arithmetic-shift opcode 11)
                      (bool->int authoritative? 10)
                      (bool->int truncated? 9)
                      (bool->int recursion-desired? 8)
                      (bool->int recursion-available? 7)
                      response-code)))
    (u16vector->bytevector/be
     (u16vector id flags question-count answer-count
                authority-count additional-count))))

(define (dns-name-as-labels name)
  (let ((end (string-length name)))
    (let lp ((start 0) (res '()))
      (if (>= start end)
          (apply bytevector-append (reverse (cons (bytevector 0) res)))
          (let ((i (string-cursor->index name (string-index name #\. start))))
            (if (> (- i start) 255)
                (error "dns name component too long" (substring name start i))
                (lp (+ i 1)
                 `(,(string->utf8 (substring name start i))
                   ,(bytevector (- i start))
                   ,@res))))))))

(define (dns-question name type class)
  (bytevector-append (dns-name-as-labels name)
                     (u16vector->bytevector/be (u16vector type class))))

(define (dns-rr name type class ttl rdata)
  (bytevector-append
   (dns-name-as-labels name)
   (u16vector->bytevector/be
    (u16vector type
               class
               (arithmetic-shift ttl -16)
               (bitwise-and ttl (- (arithmetic-shift 1 16) 1))
               (bytevector-length rdata)))
   rdata))

(define (dns-message query? opcode response-code . o)
  (let-optionals* o ((question-ls '())
                     (answer-ls '())
                     (authority-ls '())
                     (additional-ls '())
                     (authoritative? #f)
                     (truncated? #f)
                     (recursion-desired? #t)
                     (recursion-available? #f)
                     (id (random-integer dns-max-id)))
    (bytevector-append
     (dns-header id query? opcode authoritative? truncated?
                 recursion-desired? recursion-available?
                 response-code
                 (length question-ls) (length answer-ls)
                 (length authority-ls) (length additional-ls))
     (bytevector-append-map question-ls (cut apply dns-question <...>))
     (bytevector-append-map answer-ls (cut apply dns-rr <...>))
     (bytevector-append-map authority-ls (cut apply dns-rr <...>))
     (bytevector-append-map additional-ls (cut apply dns-rr <...>)))))

(define (dns-build-query opcode questions)
  (dns-message #t opcode 0 questions))

(define (dns-build-response opcode response answers)
  (dns-message #f opcode response '() answers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing

(define (read-length-string bv off len)
  (let* ((start (car off))
         (end (+ start len)))
    (set-car! off end)
    (utf8->string bv start end)))

(define (read-length-bytevector bv off len)
  (let* ((start (car off))
         (end (+ start len)))
    (set-car! off end)
    (bytevector-copy bv start end)))

(define (read-uint8 bv off)
  (let ((i (bytevector-u8-ref bv (car off))))
    (set-car! off (+ 1 (car off)))
    i))

(define (read-uint16/be bv off)
  (let* ((b1 (read-uint8 bv off))
         (b2 (read-uint8 bv off)))
    (bitwise-ior (arithmetic-shift b1 8) b2)))

(define (read-label bv off)
  (let lp ((res '()))
    (let ((i (read-uint8 bv off)))
      (cond
       ((zero? i)
        (string-join (reverse res) "."))
       ((> i 63)
        (let* ((j (read-uint8 bv off))
               (off2 (list (bitwise-ior
                            (arithmetic-shift (bitwise-and i #b111111) 8)
                            j))))
          (string-join (reverse (cons (read-label bv off2) res)) ".")))
       (else
        (lp (cons (read-length-string bv off i) res)))))))

(define (read-question bv off)
  (let* ((qname (read-label bv off))
         (qtype (read-uint16/be bv off))
         (qclass (read-uint16/be bv off)))
    (list qname qtype qclass)))

(define (inet-ntoa n)
  (string-append
   (number->string (bytevector-u8-ref n 0)) "."
   (number->string (bytevector-u8-ref n 1)) "."
   (number->string (bytevector-u8-ref n 2)) "."
   (number->string (bytevector-u8-ref n 3))))

(define (parse-rdata bv index rdata type class)
  (guard (exn
          (else
           (display "parse error: " (current-error-port))
           (write exn (current-error-port))
           (newline (current-error-port))
           rdata))
    (cond
     ((and (eqv? type dns/A) (= 4 (bytevector-length rdata)))
      (inet-ntoa rdata))
     ((eqv? type dns/NS)
      (read-label bv (list index)))
     ((eqv? type dns/MX)
      (let* ((preference (read-uint16/be rdata (list 0)))
             (host (read-label bv (list (+ index 2)))))
        (list preference host)))
     (else
      ;; (if (not (eq? type dns/TXT))
      ;;     (error "unknown type: " type class rdata))
      rdata))))

(define (read-resource bv off)
  (let* ((name (read-label bv off))
         (type (read-uint16/be bv off))
         (class (read-uint16/be bv off))
         (ttl-hi (read-uint16/be bv off))
         (ttl-lo (read-uint16/be bv off))
         (rdlength (read-uint16/be bv off))
         (data-index (car off))
         (raw-rdata (read-length-bytevector bv off rdlength))
         (rdata (parse-rdata bv data-index raw-rdata type class)))
    (list name type class (list ttl-hi ttl-lo) rdata)))

(define (read-n reader n bv off)
  (let lp ((n n) (res '()))
    (if (zero? n)
        (reverse res)
        (lp (- n 1) (cons (reader bv off) res)))))

(define (dns-parse bv)
  (let* ((off (list 0))
         (id (read-uint16/be bv off))
         (tmp1 (read-uint8 bv off))
         (tmp2 (read-uint8 bv off))
         (num-questions (read-uint16/be bv off))
         (num-answers (read-uint16/be bv off))
         (num-name-servers (read-uint16/be bv off))
         (num-additional (read-uint16/be bv off))
         (questions (read-n read-question num-questions bv off))
         (answers (read-n read-resource num-answers bv off))
         (name-servers (read-n read-resource num-name-servers bv off))
         (additional (read-n read-resource num-additional bv off)))
    (list id tmp1 tmp2 questions answers name-servers additional)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; high-level interface

;; XXXX Unix specific
(define (dns-read-resolv-conf)
  (call-with-input-file "/etc/resolv.conf"
    (lambda (in)
      (let lp ((res '()))
        (let ((line (read-line in)))
          (if (eof-object? line)
              (reverse res)
              (let ((ls (string-split line)))
                (if (string=? (car ls) "nameserver")
                    (lp (cons (cadr ls) res))
                    (lp res)))))))))

(define dns-default-name-servers
  (let ((name-servers #f))
    (lambda ()
      (or name-servers
          (let ((ns (dns-read-resolv-conf)))
            (set! name-servers ns)
            ns)))))

(define (get-udp-address-info host service)
  (let ((hints (make-address-info address-family/inet
                                  socket-type/datagram
                                  ip-proto/udp)))
    (get-address-info host service hints)))

(define (dns-query-all msg . o)
  (let query ((ns-ls (if (pair? o) (list (car o)) (dns-default-name-servers))))
    (and (pair? ns-ls)
         (guard (exn (else (query (cdr ns-ls))))
           (let* ((addr (get-udp-address-info (car ns-ls) 53))
                  (sock (socket (address-info-family addr)
                                (address-info-socket-type addr)
                                (address-info-protocol addr))))
             (send sock msg 0 addr)
             (or (receive sock 1024 0 addr)
                 (query (cdr ns-ls))))))))

(define (dns-run msg o)
  (dns-parse (apply dns-query-all msg o)))

(define (dns-lookup/full name . o)
  (dns-run (dns-build-query dns/QUERY (list (list name dns/A dns/IN))) o))
(define (dns-text/full name . o)
  (dns-run (dns-build-query dns/QUERY (list (list name dns/TXT dns/IN))) o))
(define (dns-mx/full name . o)
  (dns-run (dns-build-query dns/QUERY (list (list name dns/MX dns/IN))) o))

(define (dns-get-answer x)
  (and (list? x)
       (>= (length x) 5)
       (let ((ans-ls (list-ref x 4)))
         (and (pair? ans-ls)
              (list-ref (car ans-ls) 4)))))

(define (dns-get-answers x)
  (and (list? x)
       (>= (length x) 5)
       (map (lambda (a) (list-ref a 4)) (list-ref x 4))))

;;> Lookup and return the numeric ip address for the domain
;;> \var{name}, formatted as a string.
(define (dns-lookup name . o)
  (dns-get-answer (apply dns-lookup/full name o)))

;;> Lookup and return the DNS TEXT entry for the domain \var{name}.
(define (dns-text name . o)
  (dns-get-answer (apply dns-text/full name o)))

;;> Lookup and return the sorted list of MX entries for the domain
;;> \var{name}.
(define (dns-mx name . o)
  (sort (dns-get-answers (apply dns-mx/full name o)) < car))
