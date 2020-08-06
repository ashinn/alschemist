(define-library (chibi wiki utils)
  (import
   (scheme base) (scheme write) (srfi 1)
   (chibi string) (chibi highlight) (chibi net http) (chibi uri)
   (chibi io) (chibi filesystem) (chibi memoize) (chibi sxml)
   (chibi pathname) (chibi regexp) (chibi string)
   (chibi html-parser) (chibi html-summary))
  (export wiki-sugar wiki-word-display)
  (include "utils.scm"))
