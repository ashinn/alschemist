(define-library (chibi wiki parser-test)
  (export run-tests)
  (import (scheme base) (chibi test) (chibi wiki parser))
  (begin
    (define (run-tests)
      (test-begin "wiki parse")

      (test "wiki-word-bracket"
          '((p "What's " (wiki "call/cc" #f) "?"))
        (wiki-parse "What's [[call/cc]]?"))

      (test "wiki-word-named"
          '((p "What's " (wiki "call-with-current-continuation" "call/cc") "?"))
        (wiki-parse "What's [[call-with-current-continuation|call/cc]]?"))

      (test "multiple-wiki-words"
          '((p "To " (wiki "infinity" #f) " and " (wiki "beyond" #f) "!"))
        (wiki-parse "To [[infinity]] and [[beyond]]!"))

      (test "wiki-url"
          '((p "This is a bare external link: " (url "http://slashdot.org/" "http://slashdot.org/")))
        (wiki-parse "This is a bare external link: http://slashdot.org/"))

      (test "wiki-url-named"
          '((p "This is a named external link: " (url "http://slashdot.org/" "/.")))
        (wiki-parse "This is a named external link: [http://slashdot.org/ /.]"))

;;       (test "wiki-url-multi-line"
;;           '((p (url "http://uk.reuters.com/article/oilRpt/idUKT2250120090327"
;;                     "highway travel is so cheap")))
;;         (wiki-parse "[http://uk.reuters.com/article/oilRpt/idUKT2250120090327
;; highway travel is so cheap]"))

      (test "wiki-par" '((p "par1") (p "par2")) (wiki-parse "par1\n\npar2"))
      (test "wiki-hr" '((p "par1") (hr) (p "par2"))
        (wiki-parse "par1\n\n----\n\npar2"))
      (test "wiki-list" '((ul (li "item 1") (li "item 2")))
        (wiki-parse "* item 1\n* item 2\n\n"))
      (test "wiki-bold" '((p "This should be " (b "bold") "."))
        (wiki-parse "This should be '''bold'''."))
      (test "wiki-italic" '((p "This should be " (i "italic") "."))
        (wiki-parse "This should be ''italic''."))
      (test "wiki-underline" '((p "This should be " (u "underlined") "."))
        (wiki-parse "This should be <u>underlined</u>."))

      (test "code block"
          '((p "Check out this code:")
            (code "(define (fact n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))")
            (p "Cool, no?"))
        (wiki-parse "Check out this code:
    (define (fact n)
      (if (<= n 1)
          1
          (* n (fact (- n 1)))))
Cool, no?
"))

      (test "notes"
          '((p "In particular, the new margin-based annotations will\nmake reading and navigation much simpler."
               (note "I have\ndiscovered a truly marvellous proof of this, which this\nmargin is too narrow to contain.")))
        (wiki-parse "In particular, the new margin-based annotations will
make reading and navigation much simpler.{{I have
discovered a truly marvellous proof of this, which this
margin is too narrow to contain.}}"))

      (test-end))))
