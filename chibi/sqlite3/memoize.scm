
(define-record-type Sqlite3-Cache
  (%make-sqlite3-cache path db encode-key encode decode size-limit ttl-millis)
  sqlite3-cache?
  (path sqlite3-cache-path)
  (db sqlite3-cache-db sqlite3-cache-db-set!)
  (encode-key sqlite3-cache-encode-key)
  (encode sqlite3-cache-encode)
  (decode sqlite3-cache-decode)
  (size-limit sqlite3-cache-size-limit)
  (ttl-millis sqlite3-cache-ttl-millis))

(define (initialize-cache! cache)
  (when (not (sqlite3-cache-db cache))
    (let ((path (sqlite3-cache-path cache)))
      (create-directory* (path-directory path))
      (let ((db (sqlite3-open path)))
        (unless db
          (error "failed to open sqlite3 database" path))
        (sqlite3-exec db "CREATE TABLE IF NOT EXISTS cache (key VARCHAR(1024), value TEXT, last_read_millis BIGINT);")
        (sqlite3-exec db "CREATE UNIQUE INDEX IF NOT EXISTS cache_key ON cache (key);")
        (sqlite3-cache-db-set! cache db)))))

(define (write-to-string obj)
  (let ((out (open-output-string)))
    (write obj out)
    (let ((res (get-output-string out)))
      (close-output-port out)
      res)))

(define (read-from-string str)
  (guard (exn (else (error "couldn't read serialized object" str exn)))
    (let* ((in (open-input-string str))
           (res (read in)))
      (close-input-port in)
      res)))

(define (make-sqlite3-cache path . o)
  (let-keywords* o ((size-limit size-limit: 0)
                    (ttl-millis ttl-millis: 0)
                    (encode encode: write-to-string)
                    (decode decode: read-from-string)
                    (encode-key encode: encode))
    ;; We lazily initialize the db on the first access, since you may
    ;; want to memoize many procedures by default without ever using
    ;; them in a given program.
    (%make-sqlite3-cache path #f encode-key encode decode size-limit ttl-millis)))

(define (sqlite3-page-size db)
  (car (sqlite3-select db "SELECT page_count * page_size AS total_bytes FROM pragma_page_count(), pragma_page_size();")))

(define (sqlite3-vacuum! db)
  (sqlite3-do "VACUUM;"))

(define (sqlite3-cache-prune! cache)
  ;; iteratively remove oldest entries one at a time and vacuuming
  ;; until the page size is under the limit
  (let ((limit (sqlite3-cache-size-limit cache))
        (current-size (sqlite3-page-size (sqlite3-cache-db cache))))
    (when (positive? limit)
      'TODO)))

(define (sqlite3-cache-ref! cache key compute)
  (initialize-cache! cache)
  (let* ((key-str ((sqlite3-cache-encode-key cache) key))
         (now-millis (* 1000 (current-second)))
         (db (sqlite3-cache-db cache))
         (existing
          (sqlite3-select
           db
           "SELECT value, last_read_millis FROM cache WHERE key=?;"
           key-str)))
    (if (and (pair? existing)
             (or (zero? (sqlite3-cache-ttl-millis cache))
                 (<= (- now-millis (vector-ref (car existing) 1))
                     (sqlite3-cache-ttl-millis cache))))
        (let ((res ((sqlite3-cache-decode cache)
                    (vector-ref (car existing) 0))))
          (sqlite3-do db
                      "UPDATE cache SET last_read_millis=? WHERE key=?;"
                      now-millis
                      key-str)
          res)
        (let* ((res (compute key))
               (value-str ((sqlite3-cache-encode cache) res)))
          (sqlite3-do db "INSERT INTO cache (key, value, last_read_millis) VALUES (?, ?, ?) ON CONFLICT(key) DO UPDATE SET value=EXCLUDED.value, last_read_millis=EXCLUDED.last_read_millis " key-str value-str now-millis)
          (sqlite3-cache-prune! cache)
          res))))

;; Note the cache keys are the raw values for single argument
;; procedures, but the list of arguments for more arguments.
(define (make-sqlite3-memoizer proc arity cache)
  (case arity
    ((0)
     proc)
    ((1)
     (lambda (x) (sqlite3-cache-ref! cache x proc)))
    ((2)
     (lambda (x y)
       (sqlite3-cache-ref! cache (cons x y) (lambda (xy) (proc (car xy) (cdr xy))))))
    (else
     (lambda args
       (sqlite3-cache-ref! cache args (lambda (args) (apply proc args)))))))

;;> Similar to \scheme{memoize-to-file}, stores results for \var{proc}
;;> persistently in an sqlite3 database in \var{path}.  This is better
;;> suited when \var{proc} is called on many different arguments as it
;;> avoids creating too many individual files.  Also includes a new
;;> \scheme{ttl-millis} field.
(define (memoize/sqlite3 proc path . o)
  (let-keywords* o ((cache cache: (apply make-sqlite3-cache path o))
                    (arity arity: (and (not (procedure-variadic? proc))
                                       (procedure-arity proc)))
                    (used-args used-args: #f))
    (if (and (integer? used-args)
             (positive? used-args)
             (not (equal? used-args arity)))
        (lambda args
          (sqlite3-cache-ref! cache
                              (take args used-args)
                              (lambda (args) (apply proc args))))
        (make-sqlite3-memoizer proc arity cache))))

(define (get-memo-path name)
  (make-path
   (or (get-environment-variable "MEMOIZE_DIR")
       (make-path (or (get-environment-variable "HOME") ".")
                  ".memo.d"))
   (if (symbol? name) (symbol->string name) name)))

(define-syntax define-memoized/sqlite3
  ;; TODO: allow other keyword args
  (syntax-rules (ttl-millis:)
    ((define-memoized/sqlite3 (proc x ...) ttl-millis: ttl-millis . body)
     (define proc
       (memoize/sqlite3 (lambda (x ...) . body)
                        (get-memo-path 'proc)
                        'arity: (length '(x ...))
                        'ttl-millis: ttl-millis)))
    ((define-memoized/sqlite3 (proc x ...) . body)
     (define proc
       (memoize/sqlite3 (lambda (x ...) . body)
                        (get-memo-path 'proc)
                        'arity: (length '(x ...)))))
    ((define-memoized/sqlite3 (proc . x) ttl-millis: ttl-millis . body)
     (define proc
       (memoize/sqlite3 (lambda x . body)
                        (get-memo-path 'proc)
                        'ttl-millis: ttl-millis)))
    ((define-memoized/sqlite3 (proc . x) . body)
     (define proc
       (memoize/sqlite3 (lambda x . body)
                        (get-memo-path 'proc))))))
