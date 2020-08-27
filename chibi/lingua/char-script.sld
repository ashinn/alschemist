
(define-library (chibi lingua char-script)
  (import (scheme base))
  (export char-script)
  (include "char-script.scm")
  (begin
    ;;> Returns the script name as a symbol for \var{ch}, with most
    ;;> punctuation being in the \scheme{'common} script.
    (define (char-script ch)
      (let ((n (char->integer ch))
            (end (- (vector-length range-ends) 1)))
        (cond
         ((<= n (vector-ref range-ends 0))
          (vector-ref range-scripts 0))
         ((> n (vector-ref range-ends end))
          'unknown)
         (else
          (let lp ((lo 1) (hi end))
            (if (>= lo hi)
                (vector-ref range-scripts lo)
                (let* ((mid (quotient (+ lo hi) 2))
                       (x (vector-ref range-ends mid)))
                  (cond
                   ((< n x) (lp lo mid))
                   ((> n x) (lp (+ mid 1) hi))
                   (else (vector-ref range-scripts mid))))))))))))
