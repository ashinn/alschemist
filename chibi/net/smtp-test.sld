(define-library (chibi net smtp-test)
  (export run-tests)
  (import (scheme base) (scheme char)
          (chibi string) (chibi test) (chibi net smtp))
  (begin
    (define (run-tests)
      (test-begin "smtp")
      (test '("Message-Id:"
              "From: Dr. Watson <guest@grimpen.moor>"
              "To: Sherlock Homes <not-really@221B-baker.street>"
              "Subject: First Report"
              "Date:"
              "Mime-Version: 1.0"
              "Content-Type: text/plain; charset=\"ISO-8859-1\""
              ""
              "Moor is gloomy. Heard strange noise, attached."
              "")
          (let ((out (open-output-bytevector)))
            (smtp-write-message
             out
             'From:    "Dr. Watson <guest@grimpen.moor>"
             'To:      "Sherlock Homes <not-really@221B-baker.street>"
             'Subject: "First Report"
             'Charset: "ISO-8859-1"
             'Body:    "Moor is gloomy. Heard strange noise, attached.")
            (map (lambda (line)
                   (cond
                    ((string-prefix? "Message-Id:" line) "Message-Id:")
                    ((string-prefix? "Date:" line) "Date:")
                    (else (string-trim line char-whitespace?))))
                 (string-split (utf8->string (get-output-bytevector out))
                               #\newline))))
      (test-end))))
