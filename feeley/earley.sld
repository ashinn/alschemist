(define-library (feeley earley)
  (export make-parser parse->parsed? parse->trees parse->nb-trees tree-display)
  (import (scheme base) (scheme cxr) (scheme write))
  (include "tree.scm")
  (include "earley.scm"))
