
(define-library (chibi term sixel)
  (import (scheme base) (scheme file) (srfi 130) (srfi 151) (scheme write))
  (export-all)
  (include "sixel.scm")
  (cond-expand
   ((and chibi (not no-ffi))
    (import (chibi process))
    (begin
      ;; Calling out to img2sixel is simpler and allows the library to
      ;; work even without libsixel installed.  Also the libsixel
      ;; design is bizarre and not particularly amenable to FFI
      ;; wrapping.
      (define (load-image->sixel file)
        (let ((x (process->output+error+status (list "img2sixel" file))))
          (if (zero? (car (cddr x)))
              (car x)
              (image->sixel (load-image file)))))))
   (else
    (begin
      (define (load-image->sixel file)
        (image->sixel (load-image file)))))))
