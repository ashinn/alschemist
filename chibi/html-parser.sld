
(define-library (chibi html-parser)
  (export make-html-parser html->sxml html-strip)
  (import (scheme base) (scheme char) (scheme cxr) (scheme write))
  (include "html-parser.scm"))
