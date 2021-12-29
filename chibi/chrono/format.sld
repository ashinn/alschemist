
(define-library (chibi chrono format)
  (import (scheme base) (scheme char) (scheme list)
          (scheme sort) (scheme write) (srfi 130)
          (chibi assert) (chibi chrono base) (chibi locale) (chibi optional))
  (export temporal->string chronology-string->temporal
          chronology-temporal-parser chronology-temporal-formatter
          temporal-macro? make-temporal-macro
          unparseable? make-unparseable
          unparseable-parse unparseable-unparse
          unparseable-min-length unparseable-max-length)
  (include "format.scm"))
