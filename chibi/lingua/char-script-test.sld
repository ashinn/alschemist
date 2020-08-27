
(define-library (chibi lingua char-script-test)
  (import (scheme base) (chibi test) (chibi lingua char-script))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin)
      (test 'common (char-script #\.))
      (test 'latin (char-script #\a))
      (test 'greek (char-script #\π))
      (test 'cyrillic (char-script #\Я))
      (test 'hebrew (char-script #\א))
      (test 'arabic (char-script #\خ))
      (test 'hiragana (char-script #\あ))
      (test 'katakana (char-script #\ア))
      (test 'han (char-script #\亜))
      (test 'thai (char-script #\ก))
      (test 'hangul (char-script #\한))
      (test 'devanagari (char-script #\क))
      (test 'cherokee (char-script #\Ꮙ))
      (test 'ethiopic (char-script #\ፈ))
      (test 'tamil (char-script #\க்))
      (test-end))))
