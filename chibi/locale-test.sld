
(define-library (chibi locale-test)
  (import (scheme base) (chibi locale) (chibi test))
  (export run-tests)
  (begin
    (define (locale->list locale)
      (list (locale-language locale)
            (locale-script locale)
            (locale-region locale)
            (locale-variant locale)
            (locale-extensions locale)))
    (define (run-tests)
      (test-group "(chibi locale)"
        (test '(es #f 419 #f ())
            (locale->list (string->locale "es-419")))
        (test '(fr #f 29 #f ())
            (locale->list (string->locale "fr-029")))
        (test '(ja #f JP #f ())
            (locale->list (string->locale "ja-JP")))
        (test '(hi Latn IN #f ())
            (locale->list (string->locale "hi-Latn-IN")))
        (test '(el #f GR polyton ())
            (locale->list (string->locale "el-GR-polyton")))
        (test '(ja #f JP #f ((u . "ca-japanese")))
            (locale->list (string->locale "ja-JP-u-ca-japanese")))
        (test '(ja #f JP #f ((u . "ca-japanese") (x . "y-z")))
            (locale->list (string->locale "ja-JP-u-ca-japanese-x-y-z")))
        (test "es-419"
            (locale->string (string->locale "es-419")))
        (test "fr-029"
            (locale->string (string->locale "fr-029")))
        (test "ja-JP"
            (locale->string (string->locale "ja-JP")))
        (test "hi-Latn-IN"
            (locale->string (string->locale "hi-Latn-IN")))
        (test "el-GR-polyton"
            (locale->string (string->locale "el-GR-polyton")))
        (test "ja-JP-u-ca-japanese"
            (locale->string (string->locale "ja-JP-u-ca-japanese")))
        (test "ja-JP-u-ca-japanese-x-y-z"
            (locale->string (string->locale "ja-JP-u-ca-japanese-x-y-z")))
        (test-assert (locale= (make-locale 'hi 'IN #f 'Latn)
                              (string->locale "hi-Latn-IN")))
        (test-not (locale= (make-locale 'hi 'IN #f 'Latn)
                           (string->locale "hi-IN")))
        (test-assert (locale-includes? (make-locale 'hi 'IN #f 'Latn)
                                       (string->locale "hi-Latn-IN")))
        (test-assert (locale-includes? (make-locale 'hi 'IN)
                                       (string->locale "hi-Latn-IN")))
        (test-assert (locale-includes? (make-locale #f) locale:english))
        (test-not (locale-includes? (string->locale "hi-Latn-IN")
                                    (make-locale 'hi 'IN)))
        (test "hi-Latn-IN"
            (locale->string
             (locale-generalize (string->locale "hi-Latn-IN-x-y"))))
        (test "hi-Latn"
            (locale->string
             (locale-generalize (string->locale "hi-Latn-IN"))))
        (test "hi"
            (locale->string
             (locale-generalize (string->locale "hi-Latn"))))
        (test ""
            (locale->string
             (locale-generalize (string->locale "hi"))))
        (test-not (locale-generalize locale:root))
        ))))
