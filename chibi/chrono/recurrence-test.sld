
(define-library (chibi chrono recurrence-test)
  (import (scheme base)
          (scheme list)
          (chibi chrono)
          (chibi chrono recurrence)
          (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (define @ string->temporal)
      (define (->date-string t)
        (temporal->string t '(year "-" (fit0 2 month) "-" (fit0 2 day))))
      (define (recurrence->string-list . args)
        (map ->date-string (apply recurrence->list args)))
      (test-group "(chibi chrono recurrence)"
        (let ((rec (string->recurrence "RRULE:FREQ=DAILY;COUNT=5")))
          (test 'day (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '() (recurrence-filters rec))
          (test 5 (recurrence-count rec))
          (test #f (recurrence-start rec))
          (test #f (recurrence-end rec))
          (test '("1997-09-02" "1997-09-03" "1997-09-04"
                  "1997-09-05" "1997-09-06")
              (recurrence->string-list rec (@ "19970902T090000"))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=DAILY;UNTIL=19970907T000000Z")))
          (test 'day (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '() (recurrence-filters rec))
          (test #f (recurrence-count rec))
          (test #f (recurrence-start rec))
          (test '((year . 1997) (month . 9) (day . 7))
              (take (cdr (temporal->alist (recurrence-end rec))) 3))
          (test '("1997-09-02" "1997-09-03" "1997-09-04"
                  "1997-09-05" "1997-09-06")
              (recurrence->string-list rec (@ "19970902T090000"))))
        (let ((rec (string->recurrence "RRULE:FREQ=DAILY;INTERVAL=2")))
          (test 'day (recurrence-freq rec))
          (test 2 (recurrence-interval rec))
          (test '() (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("1997-09-02" "1997-09-04" "1997-09-06" "1997-09-08")
              (recurrence->string-list rec
                                       (@ "19970902T090000")
                                       (@ "19970910T090000"))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1")))
          (test 'day (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((month 1)) (recurrence-filters rec))
          (test #f (recurrence-count rec))
          (test #f (recurrence-start rec))
          (test '((year . 2000) (month . 1) (day . 31))
              (take (cdr (temporal->alist (recurrence-end rec))) 3))
          (let ((ls (recurrence->string-list rec (@ "19980101T090000"))))
            (test (* 31 3) (length ls))
            (test '("1998-01-01" "1998-01-02" "1998-01-03")
                (take ls 3))
            (test '("2000-01-29" "2000-01-30" "2000-01-31")
                (take-right ls 3))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;
  BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA")))
          (test 'year (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((month 1) (day-of-week 0 1 2 3 4 5 6))
              (recurrence-filters rec))
          (test #f (recurrence-count rec))
          (test #f (recurrence-start rec))
          (test '((year . 2000) (month . 1) (day . 31))
              (take (cdr (temporal->alist (recurrence-end rec))) 3))
          (let ((ls (recurrence->string-list rec (@ "19980101T090000"))))
            (test (* 31 3) (length ls))
            (test '("1998-01-01" "1998-01-02" "1998-01-03")
                (take ls 3))
            (test '("2000-01-29" "2000-01-30" "2000-01-31")
                (take-right ls 3))))
        (let ((rec (string->recurrence "RRULE:FREQ=WEEKLY")))
          (test 'week (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '() (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("1998-01-01" "1998-01-08" "1998-01-15" "1998-01-22" "1998-01-29")
              (recurrence->string-list rec (@ "19980101T090000")
                                       (@ "19980201T090000"))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=WEEKLY;WKST=SU;BYDAY=TU,TH")))
          (test 'week (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((day-of-week 2 4)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("1998-01-01" "1998-01-06" "1998-01-08" "1998-01-13" "1998-01-15" "1998-01-20" "1998-01-22" "1998-01-27" "1998-01-29")
              (recurrence->string-list rec (@ "19980101T090000")
                                       (@ "19980201T090000"))))
        ;; the first day of each quarter
        (let ((rec (string->recurrence "RRULE:FREQ=MONTHLY;INTERVAL=3;BYMONTHDAY=1")))
          (test 'month (recurrence-freq rec))
          (test 3 (recurrence-interval rec))
          (test '((day 1)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("1998-01-01" "1998-04-01" "1998-07-01" "1998-10-01"
                  "1999-01-01")
              (recurrence->string-list rec
                                       (@ "19980101T090000")
                                       (@ "19990102T090000"))))
        (let ((rec (string->recurrence "RRULE:FREQ=MONTHLY;BYDAY=1FR")))
          (test 'month (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((nth (day-of-week 5) 1)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("1998-01-02")
              (recurrence->string-list rec (@ "19980101T090000")
                                       (@ "19980201T090000"))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=1SU,-1SU")))
          (test 'month (recurrence-freq rec))
          (test 2 (recurrence-interval rec))
          (test '((nth (day-of-week 0) 1 -1)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test-skip
           "negative index test"
           (test '("1998-01-04" "1998-01-25" "1998-03-01" "1998-03-29")
               (recurrence->string-list rec (@ "19980101T090000")
                                        (@ "19980401T090000")))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=1SU,-1SU")))
          (test 'month (recurrence-freq rec))
          (test 2 (recurrence-interval rec))
          (test '((nth (day-of-week 0) 1 -1)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec)))
        (let ((rec (string->recurrence "RRULE:FREQ=MONTHLY;BYMONTHDAY=-3")))
          (test 'month (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((day -3)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec)))
        (let ((rec (string->recurrence "RRULE:FREQ=MONTHLY;BYMONTHDAY=1,-1")))
          (test 'month (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((day 1 -1)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec)))
        (let ((rec (string->recurrence "RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7")))
          (test 'year (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((month 6 7)) (recurrence-filters rec))
          (test 10 (recurrence-count rec))
          (test '("1997-06-10" "1997-07-10" "1998-06-10" "1998-07-10"
                  "1999-06-10" "1999-07-10" "2000-06-10" "2000-07-10"
                  "2001-06-10" "2001-07-10")
              (recurrence->string-list rec (@ "19970610T090000"))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200")))
          (test 'year (recurrence-freq rec))
          (test 3 (recurrence-interval rec))
          (test '((day-of-year 1 100 200)) (recurrence-filters rec))
          (test 10 (recurrence-count rec))
          (test-skip
           "expand all three day-of-years within each year"
           (test '("1997-01-01" "1997-04-10" "1997-07-19"
                   "2000-01-01" "2000-04-09" "2000-07-18"
                   "2003-01-01" "2003-04-10" "2003-07-19"
                   "2006-01-01")
               (recurrence->string-list rec (@ "19970101T090000")))))
        (let ((rec (string->recurrence "RRULE:FREQ=YEARLY;BYDAY=20MO")))
          (test 'year (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((nth (day-of-week 1) 20)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test-skip
           "nth day-of-week by year not working"
           (test '("1997-05-19" "1998-05-18" "1999-05-17")
               (recurrence->string-list rec
                                        (@ "19970519T090000")
                                        (@ "20000101T090000")))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO")))
          (test 'year (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((week-of-year 20) (day-of-week 1)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test-skip
           "day-of-week by week-of-year not working"
           (test '("1997-05-12" "1997-05-11" "1997-05-17")
               (recurrence->string-list rec
                                        (@ "19970519T090000")
                                        (@ "20000101T090000")))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13")))
          (test 'month (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((day-of-week 5) (day 13)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("1998-02-13" "1998-03-13" "1998-11-13")
              (recurrence->string-list rec
                                       (@ "19970902T090000")
                                       (@ "19990102T090000"))))
        ;; 1st Tue after a Mon in Nov, U.S. Presendential Election Day
        (let ((rec (string->recurrence
                    "RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;
  BYMONTHDAY=2,3,4,5,6,7,8")))
          (test 'year (recurrence-freq rec))
          (test 4 (recurrence-interval rec))
          (test '((month 11) (day-of-week 2) (day 2 3 4 5 6 7 8))
              (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("1996-11-05" "2000-11-07" "2004-11-02")
              (recurrence->string-list rec
                                       (@ "19960101T090000")
                                       (@ "20050101T090000"))))
        (let ((rec (string->recurrence "RRULE:FREQ=HOURLY;INTERVAL=3")))
          (test 'hour (recurrence-freq rec))
          (test 3 (recurrence-interval rec))
          (test '() (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("09:00:37" "12:00:37" "15:00:37")
              (map (lambda (t)
                     (temporal->string t '((fit0 2 hour) ":" (fit0 2 minute) ":" (fit0 2 second))))
                   (recurrence->list rec
                                     (@ "19960101T090037")
                                     (@ "19960101T180037")))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16")))
          (test 'minute (recurrence-freq rec))
          (test 20 (recurrence-interval rec))
          (test '((hour 9 10 11 12 13 14 15 16)) (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("09:00" "09:20" "09:40" "10:00" "10:20" "10:40"
                  "11:00" "11:20" "11:40" "12:00" "12:20" "12:40"
                  "13:00" "13:20" "13:40" "14:00" "14:20" "14:40"
                  "15:00" "15:20" "15:40" "16:00" "16:20" "16:40")
              (map (lambda (t)
                     (temporal->string t '((fit0 2 hour) ":" (fit0 2 minute))))
                   (recurrence->list rec
                                     (@ "19960101T090000")
                                     (@ "19960102T090000")))))
        (let ((rec (string->recurrence
                    "RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40")))
          (test 'day (recurrence-freq rec))
          (test 1 (recurrence-interval rec))
          (test '((hour 9 10 11 12 13 14 15 16) (minute 0 20 40))
              (recurrence-filters rec))
          (test-not (recurrence-bounded? rec))
          (test '("09:00" "09:20" "09:40" "10:00" "10:20" "10:40"
                  "11:00" "11:20" "11:40" "12:00" "12:20" "12:40"
                  "13:00" "13:20" "13:40" "14:00" "14:20" "14:40"
                  "15:00" "15:20" "15:40" "16:00" "16:20" "16:40")
              (map (lambda (t)
                     (temporal->string t '((fit0 2 hour) ":" (fit0 2 minute))))
                   (recurrence->list rec
                                     (@ "19960101T090000")
                                     (@ "19960102T090000")))))
        ))))
