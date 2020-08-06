(define-library (chibi html-summary)
  (export
   html-title html-summary html-remove-wiki-help html-remove-class
   html-text-only html-flatten-spans html-adjust-relative
   html-first-para html-first-sentence)
  (import
   (scheme base) (scheme char) (srfi 1)
   (chibi match) (chibi string) (chibi html-parser) (chibi sxml) (chibi uri))
  (include "html-summary.scm"))
