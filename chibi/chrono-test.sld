
(define-library (chibi chrono-test)
  (import (scheme base)
          (chibi chrono) (chibi chrono japan)
          (chibi locale) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-group "(chibi chrono)"
        (let* ((instant 1631107018)
               (dt (instant->temporal instant)))
          (test-group "basic"
            (test-assert (datetime? dt))
            (test instant
                (temporal->instant dt))
            (test `(,time-zone:utc 2021 9 8 13 16 58 0 0)
                (temporal->list dt))
            (test `(,time-zone:asia/tokyo 2021 9 8 22 16 58 0 0)
                (temporal->list
                 (instant->gregorian instant time-zone:asia/tokyo)))
            (test `(2021 9 8)
                (temporal->list
                 (instant->temporal instant chronology:gregorian-date)))
            (test `(1970 1 1)
                (temporal->list
                 (instant->temporal 0 chronology:gregorian-date)))
            (test `(1926 12 25)
                (temporal->list
                 (instant->temporal -1357635600 chronology:gregorian-date)))
            (test 220320000
                (temporal->instant (make-date 1976 12 25)))
            (test -1357603200
                (temporal->instant (make-date 1926 12 25)))
            (test `(1900 1 1)
                (temporal->list
                 (instant->temporal -2209021200 chronology:gregorian-date)))
            (test `(1643 1 4)
                (temporal->list
                 (instant->temporal -10318871939 chronology:gregorian-date)))
            (test `(1 1 1)
                (temporal->list
                 (instant->temporal -62135630339 chronology:gregorian-date)))
            (test `(1 3 3)
                (temporal->list
                 (instant->temporal -62130359939 chronology:gregorian-date)))
            (test `((time-zone . ,time-zone:utc)
                    (year . 2021) (month . 9) (day . 8)
                    (hour . 13) (minute . 16) (second . 58)
                    (nanosecond . 0) (fold . 0))
                (temporal->alist dt))
            (test 3 (temporal-ref dt 'day-of-week))
            (test 30 (temporal-ref dt 'days-in-month))
            (test 2459466 (temporal-ref dt 'julian-day))
            (test-error (temporal-update dt 'day 31))
            (test-error (temporal-update dt 'hour 24))
            (let ((dt2 (temporal-update dt 'day 29)))
              (test `((time-zone . ,time-zone:utc)
                      (year . 2021) (month . 9) (day . 29)
                      (hour . 13) (minute . 16) (second . 58)
                      (nanosecond . 0) (fold . 0))
                  (temporal->alist dt2))
              (test-error (temporal-update dt2 'month 2))
              (let ((dt3 (temporal-update dt2 'year 2020 'month 2)))
                (test `((time-zone . ,time-zone:utc)
                        (year . 2020) (month . 2) (day . 29)
                        (hour . 13) (minute . 16) (second . 58)
                        (nanosecond . 0) (fold . 0))
                    (temporal->alist dt3))))
            (test `((year . 2022) (month . 1) (day . 1))
                (temporal->alist
                 (temporal-adjust (make-date 2021 12 31) 'day 1)))
            )
          (test-group "time-zones"
            (test `(,time-zone:utc 2021 3 1 0 30 0 0 0)
                (temporal->list
                 (temporal-adjust
                  (list->temporal `(,time-zone:utc 2021 2 28 23 30 0 0 0))
                  'hour 1))))
          (test-group "formatting"
            (test "2021/9/8"
                (temporal->string dt '(year "/" month "/" day)))
            (test "2021/09/08"
                (temporal->string dt
                                  '(year "/" (fix0 2 month) "/" (fix0 2 day))))
            (test "8th of September, 2021"
                (temporal->string dt
                                  '(day (nth day) " of " month-name ", " year)))
            (test "Wednesday the 8th of September, 2021"
                (temporal->string
                 dt
                 '(day-of-week-name " the " day (nth day) " of " month-name
                                    ", " year))))
          (test-group "parsing"
            (define-syntax test-parse
              (syntax-rules ()
                ((test-parse alist str fmt)
                 (test-parse alist str fmt chronology:gregorian-date))
                ((test-parse alist str fmt chrono)
                 (test-parse alist str fmt chrono locale:english))
                ((test-parse alist str fmt chrono locale)
                 (test alist
                     (temporal->alist
                      (string->temporal str fmt chrono locale))))))
            (test-parse `((year . 2021) (month . 10) (day . 22))
                        "2021/10/22"
                        '(year "/" month "/" day))
            (test-parse `((year . 2013) (month . 12) (day . 12))
                        "2013-12-12"
                        '(year "-" month "-" day))
            (test-parse `((year . 2021) (month . 10) (day . 2))
                        "20211002"
                        '(year (fix0 2 month) (fix0 2 day)))
            (test-parse `((year . 20213) (month . 1) (day . 3))
                        "2021313"
                        '(year month day))
            (test-parse `((year . 20213) (month . 1) (day . 20))
                        "20213120"
                        '(year month day))
            (test-parse `((year . 2021) (month . 10) (day . 22))
                        "October 22 2021"
                        '(month-name " " day " " year))
            (test-parse `((year . 2021) (month . 9) (day . 8))
                        "Wednesday the 8th of September, 2021"
                        '(day-of-week-name " the " day (nth day)
                                           " of " month-name ", " year)))
          (test-group "julian"
            (test "2022/1/7"
                (temporal->string
                 (temporal-in-chronology (make-julian-date 2021 12 25)
                                         chronology:gregorian-date)
                 '(year "/" month "/" day))))
          (test-group "japanese"
            (test "55/11/20"
                (temporal->string
                 (temporal-in-chronology
                  (make-datetime time-zone:asia/tokyo 1980 11 20 0 0 0 0 0)
                  chronology:japan)
                 '(year "/" month "/" day)))
            (test "1980/11/20"
                (temporal->string
                 (temporal-in-chronology
                  (make-japanese-time 248 55 11 20)
                  chronology:gregorian-date)
                 '(year "/" month "/" day))))
          )))))
