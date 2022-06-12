
(define-library (chibi html-parser-test)
  (import (scheme base) (srfi 130) (chibi html-parser) (chibi test))
  (export run-tests)
  (begin
    (define (string-scan str pat)
      (cond
       ((string-contains str pat) => (lambda (sc) (string-cursor->index str sc)))
       (else #f)))
    (define (run-tests)
      (test-begin "(chibi html-parser)")
      (test-group "utilities"

        (test "string-scan (basic)" 3 (string-scan "abcdefghi" "def"))

        ;;(test "string-scan (case-insensitive)" 3 (string-scan "abcdEfghi" "deF"))

        (test "string-scan (single char)" 4 (string-scan "abcdefghi" "e"))

        (test "string-scan (overlap)" 12
          (string-scan "salkabcdabghabcdabdef" "abcdabd"))

        (test "string-scan (boyer-moore)" 216
          (string-scan "abracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabr"
                       "abracadabra"))

        (test "string-scan (empty pattern)" 0
          (string-scan "abakjrgaker" ""))

        )

      (test-group "parsing"

        (test '(*TOP* (a (@ (href "http://foo.scm/")) "foo"))
            (html->sxml "<a href=\"http://foo.scm/\">foo</a>"))

        (test '(*TOP* (a "a" (b "ab") "a" (c "ac")) "x")
            (html->sxml "<a>a<b>ab</b>a<c>ac</c></a>x"))

        (test '(*TOP* (p "p1") (p "p2") (p "p3"))
            (html->sxml "<p>p1<p>p2</p><p>p3"))

        (test '(*TOP* (i "italic" (b "bold italic")) (b "bold"))
            (html->sxml "<i>italic<b>bold italic</i>bold</b>"))

        (test '(*TOP* (*PI* xml "blah blah"))
            (html->sxml "<?xml blah blah?>"))

        (test '(*TOP* (*DECL* DOCTYPE HTML PUBLIC "-//W3C//DTD HTML&4.0//EN"))
            (html->sxml "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML&amp;4.0//EN\"!>"))

        (test '(*TOP* "abc" (*COMMENT* "def") "ghi")
            (html->sxml "abc<!--def-->ghi"))

        (test '(*TOP* (pre "<a>&amp;<!--foo-->"))
            (html->sxml "<pre><![CDATA[<a>&amp;<!--foo-->]]></pre>"))

        (test '(*TOP* (xmp "<a>&amp;<!--foo--><![CDATA[...]]></a>"))
            (html->sxml "<xmp><a>&amp;<!--foo--><![CDATA[...]]></a></xmp>"))

        (test '(*TOP* (b (@ (id "&")) "&"))
            (html->sxml "<b id=\"&amp;\">&amp;</b>"))

        (test '(*TOP* (foo (@ (bar "&x"))))
            (html->sxml "<foo bar=\"&x\" />"))

        (test '(*TOP* (foo (@ (bar))))
            (html->sxml "<foo bar></foo>"))

        (test '(*TOP* (div (@ (data "")) "empty"))
            (html->sxml "<div data=\"\">empty</div>"))

        (test '(*TOP* (br) "\r\n" (br) "\r\n" (div (@ (data "(sxml (@ (attr \"12345\")) body)")) "div body"))
            (html->sxml "<br>\r\n<br>\r\n<div data=\"(sxml (@ (attr &quot;12345&quot;)) body)\">div body</div>"))
        )

      (test-end))))
