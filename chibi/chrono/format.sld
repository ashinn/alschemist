
(define-library (chibi chrono format)
  (import (scheme base) (scheme char) (scheme list)
          (scheme sort) (scheme write) (srfi 130)
          (chibi assert) (chibi chrono base) (chibi locale) (chibi optional))
  (export temporal->string string->temporal
          temporal-parser temporal-formatter
          temporal-macro? make-temporal-macro
          unparseable? make-unparseable
          unparseable-parse unparseable-unparse
          unparseable-min-length unparseable-max-length)
  (include "format.scm"))
