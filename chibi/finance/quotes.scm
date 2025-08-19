
(define current-user-agent
  (make-parameter "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36"))

(define current-crumb-and-cookie
  (make-parameter (cons #f #f)))

(define (get-yahoo-crumb cookie-jar)
  (unless (and (file-exists? cookie-jar)
               (string-contains (file->string cookie-jar)
                                "yahoo.com"))
    (shell
     (curl -s -L -A ,(current-user-agent) -c ,cookie-jar
           "https://fc.yahoo.com")
     (> /dev/null)))
  (shell->string
   (curl -s -L -A ,(current-user-agent) -b ,cookie-jar
         "https://query1.finance.yahoo.com/v1/test/getcrumb")))

(define refresh-yahoo-crumb
  (opt-lambda ((crumb&cookie (current-crumb-and-cookie)))
    (cond
     ((car crumb&cookie))
     (else
      (unless (string? (cdr crumb&cookie))
        (set-cdr! crumb&cookie (temp-file "yahoo.cookies")))
      (let ((crumb (get-yahoo-crumb (cdr crumb&cookie))))
        (set-car! crumb&cookie crumb)
        crumb)))))

(define-syntax shell->json
  (syntax-rules ()
    ((shell->json cmd ...)
     (let ((str (shell->string cmd ...)))
       (log-trace "quote json: " str)
       (string->json str)))))

(define (format-uri-query base params)
  (uri->string
   (uri-with-query (if (uri? base) base (string->uri base))
                   params)))

(define quote-summary-url
  "https://query2.finance.yahoo.com/v10/finance/quoteSummary/")

;; TODO: make this configurable, we don't usually need all of these
;; details.
(define stock-modules
  "financialData,quoteType,defaultKeyStatistics,assetProfile,price,summaryDetail")

(define (get-live-stock-quote symbol)
  (let* ((crumb&cookie (current-crumb-and-cookie))
         (crumb (refresh-yahoo-crumb crumb&cookie)))
    (log-debug "yahoo crumb: " crumb)
    (let ((res
           (shell->json
            (curl -s -L -A ,(current-user-agent) -b ,(cdr crumb&cookie)
                  ,(format-uri-query
                    (string-append quote-summary-url (symbol->string symbol))
                    `(("corsDomain" . "finance.yahoo.com")
                      ("formatted" . "false")
                      ("symbol" . ,(symbol->string symbol))
                      ("modules" . ,stock-modules)
                      ("crumb" . ,crumb)))))))
      (match res
        ((('quoteSummary ('result . #(data)) . x))
         ;; Unwrap just the result data.
         data)
        ((('quoteSummary ('result . #(data0 data1 ...)) . x))
         (cons data0 data1))
        (((x ... ('error . msg)))
         ;; TODO: Regenerate cookies and crumbs on error for
         ;; long-running programs.
         (error "error getting stock quote" msg))
        (else
         (error "malformed result" res))))))

;;> Returns an alist following the Yahoo! Finance API for the given
;;> stock symbol.  The optional crumb can be generated from
;;> \scheme{get-yahoo-crumb} and reused for multiple requests.
;;> To just get the latest price you can use \scheme{get-stock-price}.
(define-memoized/sqlite3 (get-stock-quote symbol)
  ;; default 1 hour TTL
  ttl-millis: (or (cond ((get-environment-variable "QUOTE_TTL_MILLIS")
                         => string->number)
                        (else #f))
                  (* 60 60 1000))
  (get-live-stock-quote symbol))

(define (extract-price info)
  (cond
   ((assq 'price info)
    => (lambda (ls)
         (cond ((assq 'regularMarketPrice (cdr ls)) => cdr)
               (else (error "malformed price data" info)))))
   (else
    (error "malformed stock data" info))))

(define (get-stock-price symbol)
  (extract-price
   (if (symbol? symbol) (get-stock-quote symbol) symbol)))

(define (extract-dividend-yield info)
  (cond
   ((assq 'summaryDetail info)
    => (lambda (ls)
         (cond ((assq 'trailingAnnualDividendYield (cdr ls)) => cdr)
               (else (error "malformed summary data" info)))))
   (else
    (error "malformed stock data" info))))

(define (get-stock-dividend-yield symbol)
  (extract-dividend-yield
   (if (symbol? symbol) (get-stock-quote symbol) symbol)))

(define (format-currency-symbol from to)
  (string->symbol
   (string-append (symbol->string from) (symbol->string to) "=X")))

(define (get-exchange-quote from to)
  (get-stock-quote (format-currency-symbol from to)))

(define (get-exchange-rate from to)
  (extract-price (get-exchange-quote from to)))
