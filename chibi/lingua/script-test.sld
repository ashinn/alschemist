
(define-library (chibi lingua script-test)
  (import (scheme base)
          (chibi char-set)
          (chibi lingua script)
          (chibi test))
  (export run-tests)
  (begin
    (define (test-char-script ch script)
      (test script (char-script ch))
      (test-assert (char-set-contains? (script->char-set script) ch)))
    (define (run-tests)
      (test-begin)
      (test-char-script #\. 'common)
      (test-char-script #\a 'latin)
      (test-char-script #\π 'greek)
      (test-char-script #\Я 'cyrillic)
      (test-char-script #\א 'hebrew)
      (test-char-script #\خ 'arabic)
      (test-char-script #\あ 'hiragana)
      (test-char-script #\ア 'katakana)
      (test-char-script #\亜 'han)
      (test-char-script #\ก 'thai)
      (test-char-script #\한 'hangul)
      (test-char-script #\क 'devanagari)
      (test-char-script #\Ꮙ 'cherokee)
      (test-char-script #\ፈ 'ethiopic)
      (test-char-script #\க 'tamil)
      (test-end))))
