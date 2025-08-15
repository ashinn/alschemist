
(define-record-type Sqlite3-Cache
  (%make-sqlite3-cache path db encode-key encode decode size-limit ttl-millis)
  sqlite3-cache?
  (path sqlite3-cache-path)
  (db sqlite3-cache-db)
  (encode-key sqlite3-cache-encode-key)
  (encode sqlite3-cache-encode)
  (decode sqlite3-cache-decode)
  (size-limit sqlite3-cache-size-limit)
  (ttl-millis sqlite3-cache-ttl-millis))

(define (initialize-cache-db! db)
  (sqlite3-exec db "CREATE TABLE IF NOT EXISTS cache (key VARCHAR(1024), value TEXT, last_read_millis BIGINT);")
  (sqlite3-exec db "CREATE UNIQUE INDEX cache_key ON cache (key);"))

(define (write-to-string obj)
  (let ((out (open-output-string)))
    (write obj out)
    (let ((res (get-output-string out)))
      (close-output-port out)
      res)))

(define (read-from-string str)
  (let* ((in (open-input-string str))
         (res (read in)))
    (close-input-port in)
    res))

(define (make-sqlite3-cache path . o)
  (let-keywords* o ((size-limit size-limit: 0)
                    (ttl-millis ttl-millis: 0)
                    (encode encode: write-to-string)
                    (decode decode: read-from-string)
                    (encode-key encode: encode))
    (let ((db (sqlite3-open path)))
      (initialize-cache-db! db)
      (%make-sqlite3-cache path db encode-key encode decode size-limit ttl-millis))))

(define (sqlite3-page-size db)
  (car (sqlite3-select db "SELECT page_count * page_size AS total_bytes FROM pragma_page_count(), pragma_page_size();")))

(define (sqlite3-vacuum! db)
  (sqlite3-do "VACUUM;"))

(define (sqlite3-cache-prune! cache)
  ;; iteratively remove oldest entries one at a time and vacuuming
  ;; until the page size is under the limit
  (let ((limit (sqlite3-cache-size-limit cache)))
    (when (positive? limit)
      'TODO)))

(define (sqlite3-cache-ref! cache key compute)
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

;;> Similar to \scheme{memoize-to-file}, stores results for \var{proc}
;;> persistently in an sqlite3 database in \var{path}.  This is better
;;> suited when \var{proc} is called on many different arguments as it
;;> avoids creating too many individual files.  Also includes a new
;;> \scheme{ttl-millis} field.
(define (memoize/sqlite3 proc path . o)
  (let ((cache (apply make-sqlite3-cache path o))
        (arity (cond ((memq 'arity: o) => cadr)
                     (else (and (not (procedure-variadic? proc))
                                (procedure-arity proc))))))
    (make-memoizer proc arity cache sqlite3-cache-ref!)))
