(define-library (jkode sassy-test)
  (export run-tests)
  (import (scheme base) (jkode sassy) (chibi test))
  (begin
    (define-syntax test-sassy
      (syntax-rules ()
        ((test-sassy expected code)
         (test `expected (sassy-text-list (sassy `code))))))
    (define (run-tests)
      (test-begin "(jkode sassy)")
      (test-sassy
       (184 1 0 0 0 187 0 0 0 0 205 128)
       ((entry _start)
        (text
         (label _start 
                (begin (mov eax 1)
                       (mov ebx 0)
                       (int #x80))))))
      (test-end))))
