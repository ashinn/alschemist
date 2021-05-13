
;;> A wrapper around MeCab, a part-of-speech and morphological
;;> analyzer for Japanese.
;;>
;;> \scheme{
;;> (let ((mecab (make-mecab "/var/lib/mecab/dic/ipadic-utf8/")))
;;>   (mecab-parse mecab "今日学校に行きます"))
;;> => (("今日" "名詞,時相名詞,*,*,今日,きょう,代表表記:今日/きょう カテゴリ:時間") ...)
;;> }
;;>
;;> \scheme{
;;> (mecab-tokenize "今日学校に行きます")
;;> => ("今日" "学校" "に" "行き" "ます" "\n")
;;> }
;;>
;;> \scheme{
;;> (mecab-yomi "今日学校に行きます")
;;> => "きょうがっこうにいきます\n"
;;> }
;;>
;;> Note in addition to the mecab library you will need a dictionary
;;> in utf-8 format installed, e.g. in Ubuntu from the
;;> mecab-ipadic-utf8 or mecab-jumandic-utf8 packages.

(define-library (chibi mecab)
  (import (scheme base) (srfi 130) (chibi assert) (chibi optional))
  (export
   mecab-dictionary-path
   mecab-tokenize mecab-tokenizer
   mecab-yomi mecab-pronouncer
   mecab? make-mecab mecab-version mecab-strerror
   mecab-parse mecab-get-partial mecab-set-partial
   mecab-get-theta mecab-set-theta
   mecab-get-lattice-level mecab-set-lattice-level
   mecab-get-all-morphs mecab-set-all-morphs
   mecab-last-error
   %make-mecab %mecab-parse)
  (include-shared "mecab")
  (begin
    ;;> A parameter holding the default path of the dictionary to use
    ;;> for \scheme{make-mecab}.
    (define mecab-dictionary-path
      (make-parameter "/var/lib/mecab/dic/juman-utf8/"
                      (lambda (x) (assert (string? x)) x)))
    ;;> Create a new mecab parser.
    (define (make-mecab . o)
      (let-keywords* o ((path (mecab-dictionary-path))
                        (format ""))
        (%make-mecab (list "mecab" "-O" format "-d" path))))
    ;;> Parses the string \var{str} with the \var{mecab} parser, and
    ;;> returns a list of parses.
    (define (mecab-parse mecab str)
      (map (lambda (x) (string-split x "\t"))
           (string-split (%mecab-parse mecab str) "\n")))
    ;;> A parameter holding the mecab parser used for
    ;;> \scheme{mecab-tokenize}.
    (define mecab-tokenizer
      (make-parameter #f))
    ;;> Splits \var{str} into a list of tokens.
    (define (mecab-tokenize str)
      (unless (mecab-tokenizer)
        (mecab-tokenizer (make-mecab 'format: "wakati")))
      (string-split (%mecab-parse (mecab-tokenizer) str) " "))
    ;;> A parameter holding the mecab parser used for
    ;;> \scheme{mecab-yomi}.
    (define mecab-pronouncer
      (make-parameter #f))
    ;;> Returns the hiragana pronunciation of \var{str}.
    (define (mecab-yomi str)
      (unless (mecab-pronouncer)
        (mecab-pronouncer (make-mecab 'format: "yomi")))
      (%mecab-parse (mecab-pronouncer) str))))
