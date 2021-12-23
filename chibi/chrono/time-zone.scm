
(define-record-type Time-Zone
  (%make-time-zone name offset get-additional-offset)
  time-zone?
  (name time-zone-name)
  (offset time-zone-offset)
  (get-additional-offset time-zone-get-additional-offset))

(define (make-time-zone name offset . o)
  (%make-time-zone name offset (and (pair? o) (car o))))

(define time-zone:utc
  (make-time-zone "Etc/UTC" 0))

(define time-zone:asia/tokyo
  (make-time-zone "Asia/Tokyo" (* 9 60)))

;; Rule   US   1918   1919   -   Mar   lastSun   2:00   1:00   D
;; Rule   US   1918   1919   -   Oct   lastSun   2:00   0   S
;; Rule   US   1942   only   -   Feb   9   2:00   1:00   W # War
;; Rule   US   1945   only   -   Aug   14   23:00u   1:00   P # Peace
;; Rule   US   1945   only   -   Sep   30   2:00   0   S
;; Rule   US   1967   2006   -   Oct   lastSun   2:00   0   S
;; Rule   US   1967   1973   -   Apr   lastSun   2:00   1:00   D
;; Rule   US   1974   only   -   Jan   6   2:00   1:00   D
;; Rule   US   1975   only   -   Feb   lastSun   2:00   1:00   D
;; Rule   US   1976   1986   -   Apr   lastSun   2:00   1:00   D
;; Rule   US   1987   2006   -   Apr   Sun>=1   2:00   1:00   D
;; Rule   US   2007   max    -   Mar   Sun>=8   2:00   1:00   D
;; Rule   US   2007   max    -   Nov   Sun>=1   2:00   0   S

;; [EST]2ndSunOfMar@2am[EDT]1stSunOfNov@2am[EST+fold]1stSunOfNov@2am'[EST]
;; [EST]2ndSunOfMar@7am-u[EDT]1stSunOfNov@6am-u[EST+fold]1stSunOfNov@7am-u[EST]
(define time-zone:america/new-york
  (make-time-zone "America/New_York"
                  (* 5 60)
                  ;; (lambda (date)
                  ;;   (if (date<= Mar date Nov) -60 0))
                  ))

;; current offset from utc
(define (temporal-offset t)
  (let ((tz (temporal-ref t 'time-zone)))
    (+ (time-zone-offset tz)
       (cond
        ((time-zone-get-additional-offset tz)
         => (lambda (get-add)
              (let-values (((extra-offset fold) (get-add t)))
                extra-offset)))
        (else 0)))))

(define (temporal-in-utc t)
  (if (eq? time-zone:utc (temporal-ref t 'time-zone))
      t
      (let ((offset (temporal-offset t)))
        (temporal-update (temporal-adjust t 'minute (- offset))
                         'time-zone time-zone:utc))))

(define (temporal-in-time-zone t tz)
  (if (eq? tz (temporal-ref t 'time-zone))
      t
      (let ((t (temporal-in-utc t))
            (offset (time-zone-offset tz)))
        (cond
         ((time-zone-get-additional-offset tz)
          => (lambda (get-add)
               (let-values (((extra-offset fold) (get-add t)))
                 (temporal-update
                  (temporal-adjust t 'minute (+ offset extra-offset))
                  'time-zone tz
                  'fold fold))))
         (else
          (temporal-update (temporal-adjust t 'minute offset)
                           'time-zone tz))))))
