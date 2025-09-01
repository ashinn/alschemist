
;;> A schedule has a \var{name}, an arbitrary associated object
;;> \var{what}, a temporal \var{when} the schedule takes place, and a
;;> procedure to generate the \var{next} occurrence, called as
;;> \scheme{(next what when)}, returning \scheme{#f} to indicate no
;;> more occurrences.
(define-record-type Schedule
  (make-schedule name what when next)
  schedule?
  (name schedule-name schedule-name-set!)
  (what schedule-what schedule-what-set!)
  (when schedule-when schedule-when-set!)
  (next schedule-next schedule-next-set!))

;;> A time-table is an ordered set of schedules, with utilities for
;;> iterating over their events.  This allows you to abstractly walk
;;> through time independently of the current time.
(define-record-type Time-Table
  (%make-time-table schedules)
  time-table?
  (schedules time-table-schedules time-table-schedules-set!))

(define (schedule=? a b)
  (and (equal? (schedule-name a) (schedule-name b))
       (temporal=? (schedule-when a) (schedule-when b))
       (equal? (schedule-what a) (schedule-what b))))

(define (schedule<? a b)
  (or (temporal<? (schedule-when a) (schedule-when b))
      (and (temporal<=? (schedule-when a) (schedule-when b))
           (or (string<? (schedule-name a) (schedule-name b))
               (and (string=? (schedule-name a) (schedule-name b))
                    (< (hash (schedule-what a)) (hash (schedule-what b))))))))

(define schedule-resolve-when
  (opt-lambda (schedule (chronology chronology:gregorian))
    (if (schedule-when schedule)
        schedule
        (make-schedule (schedule-name schedule)
                       (schedule-what schedule)
                       (instant->temporal (current-second) chronology)
                       (schedule-next schedule)))))

(define (make-time-table)
  (%make-time-table
   (mapping (make-comparator schedule? schedule=? schedule<? hash))))

(define (time-table-empty? tt)
  (mapping-empty? (time-table-schedules tt)))

(define (time-table-add tt schedule)
  (if (not schedule)
      tt
      (let ((schedule (schedule-resolve-when schedule)))
        (%make-time-table
         (mapping-set (time-table-schedules tt)
                      schedule
                      schedule)))))

(define (time-table-add! tt schedule)
  (let ((schedule (schedule-resolve-when schedule)))
    (time-table-schedules-set! tt
                               (mapping-set (time-table-schedules tt)
                                            schedule
                                            schedule))
    tt))

(define (list->time-table schedules)
  (let lp ((tt (make-time-table))
           (ls schedules))
    (cond
     ((null? ls)
      tt)
     ((null? (car ls))
      (lp tt (cdr ls)))
     ((pair? (car ls))
      (lp (lp tt (car ls)) (cdr ls)))
     (else
      (lp (time-table-add tt (car ls))
          (cdr ls))))))

(define (time-table . o)
  (list->time-table o))

(define (time-table-next tt)
  (call-with-values
      (lambda () (mapping-pop (time-table-schedules tt)))
    (lambda (rest when schedule)
      (values schedule (%make-time-table rest)))))

;;> The fundamental time-table iterator, folds over all scheduled
;;> events until time \var{until}, accumulating the value from
;;> \scheme{(kons when what acc)}.  Returns two values, the final
;;> accumulator and a time-table for the remaining schedules.
(define (time-table-project tt until kons knil)
  (let lp ((tt tt)
           (acc knil))
    (cond
     ((time-table-empty? tt)
      (values acc tt))
     (else
      (call-with-values
          (lambda () (time-table-next tt))
        (lambda (schedule tt2)
          (cond
           ((temporal>? (schedule-when schedule) until)
            (values acc tt2))
           (else
            (let ((acc (kons (schedule-when schedule)
                             (schedule-what schedule)
                             acc))
                  (next ((schedule-next schedule)
                         (schedule-what schedule)
                         (schedule-when schedule))))
              (if next
                  (if (temporal<=? (schedule-when next)
                                   (schedule-when schedule))
                      (error "no progress in schedule" schedule next)
                      (lp (time-table-add tt2 next) acc))
                  (lp tt2 acc)))))))))))

;;> Just calls (\var{proc} temporal) on each scheduled event in order.
(define (time-table-run tt until)
  (time-table-project tt until
                      (lambda (date proc acc)
                        ;; (log-info (temporal->string date) ": "
                        ;;           (procedure-name proc))
                        (proc date)
                        #f)
                      #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generating schedules

;;> Returns a schedule for the given recurrence.
(define recurrence->schedule
  (opt-lambda (rec (name "recurrence") (what #f) (start #f) (end #f))
    (letrec* ((gen-next (recurrence->generator rec start end))
              (next
               (lambda (what when)
                 (cond ((gen-next)
                        => (lambda (t) (make-schedule name what t next)))
                       (else #f)))))
      (next what (or start (recurrence-start rec))))))

(define (limited-schedule next end)
  (if end
      (lambda (what when)
        (let ((schedule (next what when)))
          (and (temporal<? (schedule-when schedule) end)
               schedule)))
      next))

;;> Takes a list of schedules optionally interleaved with temporals or
;;> durations, and ensures the schedules run sequentially, and each
;;> has a resolved start, and all but the last have an end.
(define (time-line . args)
  (let lp ((ls args)
           (res '())
           (from #f))
    (cond
     ((null? ls)
      (reverse res))
     ((null? (car ls))
      (lp (cdr ls) res from))
     ((pair? (car ls))
      (lp (append (car ls) (cdr ls)) res from))
     ((schedule? (car ls))
      (let ((res
             (cons
              (cond
               ((schedule-when (car ls))
                (car ls))
               (else
                (schedule-resolve-when
                 (make-schedule (schedule-name (car ls))
                                (schedule-what (car ls))
                                (or from (and (pair? res)
                                              (schedule-when (car res))))
                                (schedule-next (car ls))))))
              res)))
        (lp (cdr ls) res #f)))
     ((temporal? (car ls))
      (lp (cdr ls)
          (if (pair? res)
              (cons
               (letrec ((next
                         (limited-schedule
                          (lambda (what when)
                            (let ((schedule
                                   ((schedule-next (car res)) what when)))
                              (and schedule
                                   (make-schedule (schedule-name schedule)
                                                  (schedule-what schedule)
                                                  (schedule-when schedule)
                                                  next))))
                          (car ls))))
                 (make-schedule (schedule-name (car res))
                                (schedule-what (car res))
                                (schedule-when (car res))
                                next))
               (cdr res))
              res)
          (car ls)))
     ((and (duration? (car ls)) from)
      (lp (cons (temporal-add-duration from (car ls)) (cdr ls))
          res
          #f))
     ((and (duration? (car ls)) (null? res))
      (lp (cdr ls) res from))
     ((duration? (car ls))
      (lp (cons (temporal-add-duration (schedule-when (car res)) (car ls))
                (cdr ls))
          res
          #f))
     (else
      (error "expected a schedule, temporal or duration but got" (car ls))))))

;; The following convenience utilities generate simple schedules
;; without relying on full recurrences.

;;> Returns a schedule which runs only once.
(define make-one-time-schedule
  (opt-lambda (name what (start #f))
    (make-schedule name what start (lambda (what when) #f))))

;;> Returns a schedule which runs in fixed duration increments.
(define make-duration-schedule
  (opt-lambda (name duration what (start #f) (end #f))
    (letrec
        ((next
          (limited-schedule
           (lambda (what when)
             (make-schedule name what (temporal-add-duration when duration) next))
           end)))
      (make-schedule name what start next))))

;;> Returns a schedule which runs daily.
(define make-daily-schedule
  (opt-lambda (name what (start #f) (end #f))
    (letrec ((next
              (limited-schedule
               (lambda (what when)
                 (make-schedule name what (temporal-adjust when 'day 1) next))
               end)))
      (make-schedule name what start next))))

;;> Returns a schedule which runs weekly.
(define make-weekly-schedule
  (opt-lambda (name what (start #f) (end #f))
    (letrec ((next
              (limited-schedule
               (lambda (what when)
                 (make-schedule name what (temporal-adjust when 'day 7) next))
               end)))
      (make-schedule name what start next))))

;;> Returns a schedule which runs monthly.
(define make-monthly-schedule
  (opt-lambda (name what (start #f) (end #f))
    (letrec ((next
              (limited-schedule
               (lambda (what when)
                 (make-schedule name what (temporal-adjust when 'month 1) next))
               end)))
      (make-schedule name what start next))))

;;> Returns a schedule which runs quarterly.
(define make-quarterly-schedule
  (opt-lambda (name what (start #f) (end #f))
    (letrec ((next
              (limited-schedule
               (lambda (what when)
                 (make-schedule name what (temporal-adjust when 'month 3) next))
               end)))
      (make-schedule name what start next))))

;;> Returns a schedule which runs yearly.
(define make-yearly-schedule
  (opt-lambda (name what (start #f) (end #f))
    (letrec ((next
              (limited-schedule
               (lambda (what when)
                 (make-schedule name what (temporal-adjust when 'year 1) next))
               end)))
      (make-schedule name what start next))))
