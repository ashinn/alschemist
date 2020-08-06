
(define-library (chibi coding bwt-test)
  (import (scheme base) (chibi coding bwt) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "burrows–wheeler transform")
      (test "\x03;ANNB\x02;AA"  ;; wp: "BNN\x02;AA\x03;A"
          (block-sort "BANANA"))
      (test "BANANA"
          (invert-block-sort "\x03;ANNB\x02;AA"))
      (test-end))))
