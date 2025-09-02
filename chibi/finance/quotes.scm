
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

(define (->currency-code x)
  (symbol->string (if (currency? x) (currency-code x) x)))

(define (format-currency-code from to)
  (string->symbol
   (string-append (->currency-code from) (->currency-code to) "=X")))

(define (get-exchange-quote from to)
  (get-stock-quote (format-currency-code from to)))

(define (get-exchange-rate from to)
  (extract-price (get-exchange-quote from to)))

(define quote-history-url
  "https://data.nasdaq.com/api/v3/datatables/WIKI/PRICES.csv")

(define nasdaq-api-key
  (make-parameter (or (get-environment-variable "NASDAQ_DL_API_KEY") "")))

(define (get-live-stock-history symbol)
  (assert (and (string? (nasdaq-api-key)) (not (equal? "" (nasdaq-api-key))))
          "A Nasdaq Data Link API KEY is required for stock history (set your NASDAQ_DL_API_KEY env var)")
  (let ((res
         (map
          (lambda (x)
            (list->vector
             (map (lambda (c) (or (string->number c) c))
                  (string-split x ","))))
          (shell->string-list
           (curl -s -L -A ,(current-user-agent)
                 ,(format-uri-query
                   quote-history-url
                   `(("ticker" . ,(symbol->string symbol))
                     ("api_key" . ,(nasdaq-api-key)))))))))
    (cons (vector-map string->symbol (car res))
          (reverse (cdr res)))))

(define-memoized/sqlite3 (get-stock-history symbol)
  ;; default 1 month TTL
  ttl-millis: (or (cond ((get-environment-variable "QUOTE_HISTORY_TTL_MILLIS")
                         => string->number)
                        (else #f))
                  (* 30 24 60 60 1000))
  (get-live-stock-history symbol))

(define (temporal-diff/years end start)
  ;; TODO: utility to subtract temporals and get a duration
  (/ (- (temporal->instant end)
        (temporal->instant start))
     (* 365.25 24 60 60)))

;; returns the CAGR for the full period and the monthly volatility
(define (extract-cagr quotes)
  (let ((price-field-index
         (or (vector-index (lambda (x) (eq? x 'adj_close)) (car quotes))
             (vector-index (lambda (x) (eq? x 'close)) (car quotes))
             (vector-index number? (cadr quotes))
             (error "couldn't find price column" (car quotes) (cadr quotes))))
        (date-field-index
         (or (vector-index (lambda (x) (eq? x 'date)) (car quotes))
             (vector-index (lambda (x)
                             (guard (exn (else #f))
                               (string->temporal x)))
                           (cadr quotes))
             (error "couldn't find date column" (car quotes) (cadr quotes)))))
    (define (extract-monthly-diffs ls res cur-month start-price price)
      (cond
       ((null? ls)
        (let ((res (if cur-month (cons (- price start-price) res) res)))
          (reverse res)))
       (else
        (let ((month (substring (vector-ref (car ls) date-field-index)
                                0 7))
              (new-price (vector-ref (car ls) price-field-index)))
          (cond
           ((equal? month cur-month)
            (extract-monthly-diffs (cdr ls) res cur-month start-price new-price))
           (else
            (let ((res (if cur-month (cons (- price start-price) res) res)))
              (extract-monthly-diffs (cdr ls) res month new-price new-price))))))))
    (let ((start (string->temporal (vector-ref (cadr quotes) date-field-index)))
          (start-price (vector-ref (cadr quotes) price-field-index))
          (end (string->temporal (vector-ref (last quotes) date-field-index)))
          (end-price (vector-ref (last quotes) price-field-index)))
      (values (- (exp (/ (log (- end-price start-price))
                         (temporal-diff/years end start)))
                 1)
              (stdev (extract-monthly-diffs (cdr quotes) '() #f 0 0))))))

(define (get-stock-cagr symbol)
  (extract-cagr
   (if (symbol? symbol) (get-stock-history symbol) symbol)))
