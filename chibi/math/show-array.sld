
(define-library (chibi math show-array)
  (import (scheme base)
          (scheme write)
          (srfi 166 base)
          (srfi 231)
          (chibi math dual)
          (chibi show shared))
  (export show* written-array)
  (begin
    (define array-type-tag
      (let ((storage-tags
             `((,u1-storage-class . "u1")
               (,u8-storage-class . "u8")
               (,u16-storage-class . "u16")
               (,u32-storage-class . "u32")
               (,u64-storage-class . "u64")
               (,s8-storage-class . "s8")
               (,s16-storage-class . "s16")
               (,s32-storage-class . "s32")
               (,s64-storage-class . "s64")
               (,f32-storage-class . "f32")
               (,f64-storage-class . "f64")
               (,c64-storage-class . "c64")
               (,c128-storage-class . "c128"))))
        (lambda (a)
          (cond ((and (specialized-array? a)
                      (assq (array-storage-class a) storage-tags))
                 => cdr)
                (else "a")))))

    ;; TODO: need better factoring to override the output for one type
    (define (write-array-with-shares obj shares)
      (fn ((orig-radix radix) precision)
        (let ((write-number
               ;; Shortcut for numeric values.  Try to rely on
               ;; number->string for standard radixes and no precision,
               ;; otherwise fall back on numeric but resetting to a usable
               ;; radix.
               (cond
                ((and (not precision)
                      (assv orig-radix
                            '((16 . "#x") (10 . "") (8 . "#o") (2 . "#b"))))
                 => (lambda (cell)
                      (lambda (n)
                        (cond
                         ((eqv? orig-radix 10)
                          (displayed (number->string n (car cell))))
                         ((exact? n)
                          (each (cdr cell) (number->string n (car cell))))
                         (else
                          (with ((radix 10)) (numeric n)))))))
                (else (lambda (n) (with ((radix 10)) (numeric n)))))))
          ;; `wr' is the recursive writer closing over the shares.
          (let wr ((obj obj))
            (call-with-shared-ref
             obj shares each
             (fn ()
               (cond
                ((pair? obj)
                 (each "("
                       (fn ()
                         (let lp ((ls obj))
                           (let ((rest (cdr ls)))
                             (each (wr (car ls))
                                   (cond
                                    ((null? rest)
                                     nothing)
                                    ((pair? rest)
                                     (each
                                      " "
                                      (call-with-shared-ref/cdr
                                       rest shares each
                                       (fn () (lp rest)))))
                                    (else
                                     (each " . " (wr rest))))))))
                       ")"))
                ((vector? obj)
                 (let ((len (vector-length obj)))
                   (if (zero? len)
                       (displayed "#()")
                       (each "#("
                             (wr (vector-ref obj 0))
                             (fn ()
                               (let lp ((i 1))
                                 (if (>= i len)
                                     nothing
                                     (each " " (wr (vector-ref obj i))
                                           (fn () (lp (+ i 1)))))))
                             ")"))))
                ((array? obj)
                 (each "#" (number->string (array-dimension obj))
                       (array-type-tag obj)
                       (if (zero? (array-dimension obj)) " " "")
                       (array->list* obj)))
                ((dual? obj)
                 (each "{Dual " (dual-value obj) "}"))
                ((number? obj)
                 (write-number obj))
                (else
                 (let ((out (open-output-string)))
                   (write obj out)
                   (displayed (get-output-string out)))))))))))

    ;;> Like \scheme{written-default} but with SRFI 163 output for arrays.
    (define (written-array obj)
      (fn ()
        (write-array-with-shares obj (extract-shared-objects obj #t))))

    ;;> Like \scheme{show} but with SRFI 163 output for arrays.
    (define (show* out . args)
      (show out
            (with ((written written-array))
              (each-in-list args))))))
