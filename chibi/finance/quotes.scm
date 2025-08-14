
(define (temp-file template)
  (call-with-temp-file
      template
    (lambda (path out keep!)
      (keep!)
      path)))

(define current-user-agent
  (make-parameter "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"))

(define current-cookie-jar
  (make-parameter (delay (temp-file "yahoo.cookies"))))

(define get-yahoo-crumb
  (opt-lambda (cookie-jar (force (current-cookie-jar)))
    (unless (and (file-exists? cookie-jar)
                 (string-contains (file->string cookie-jar)
                                  "yahoo.com"))
      (shell
       (curl -s -L -A ,(current-user-agent) -c ,cookie-jar
             "https://fc.yahoo.com")
       (> /dev/null)))
    (shell->string
     (curl -s -L -A ,(current-user-agent) -b ,cookie-jar
           "https://query1.finance.yahoo.com/v1/test/getcrumb"))))

(define-syntax shell->json
  (syntax-rules ()
    ((shell->json cmd ...)
     (string->json (shell->string cmd ...)))))

(define (format-uri-query base params)
  (uri->string
   (uri-with-query (if (uri? base) base (string->uri base))
                   params)))

(define quote-summary-url
  "https://query2.finance.yahoo.com/v10/finance/quoteSummary/")

(define stock-modules
  "financialData,quoteType,defaultKeyStatistics,assetProfile,summaryDetail")

;;> Returns an alist following the Yahoo! Finance API for the given
;;> stock symbol.  The optional crumb can be generated from
;;> \scheme{get-yahoo-crumb} and reused for multiple requests.
;;> To just get the latest price you can use:
;;> \scheme{(assq 'open (cdr (assq 'summaryDetail (get-stock-quote sym))))}
(define get-stock-quote
  (opt*-lambda (symbol
                (crumb #f)
                (cookie-jar (current-cookie-jar)))
    (let ((crumb (or crumb (get-yahoo-crumb (force cookie-jar)))))
      (log-debug "crumb: " crumb)
      (let ((res
             (shell->json
              (curl -s -L -A ,(current-user-agent) -b ,(force cookie-jar)
                    ,(format-uri-query
                      (string-append quote-summary-url (symbol->string symbol))
                      `(("corsDomain" . "finance.yahoo.com")
                        ("formatted" . "false")
                        ("symbol" . ,(symbol->string symbol))
                        ("modules" . ,stock-modules)
                        ("crumb" . ,crumb)))))))
        ;; Unwrap just the result data.
        (match res
          ((('quoteSummary ('result . #(data)) . x))
           data)
          ((('quoteSummary ('result . #(data0 data1 ...)) . x))
           (cons data0 data1))
          (((x ... ('error . msg)))
           (error "error getting stock quote" msg))
          (else
           (error "malformed result" res)))))))
