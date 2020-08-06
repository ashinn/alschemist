
(define-library (chibi coding huffman)
  (export make-huffman-code make-huffman-stats
          huffman-encode huffman-decode
          huffman-decode-string huffman-decode-vector)
  (import (scheme base) (scheme bitwise) (scheme hash-table)
          (scheme list) (scheme sort) (scheme write))
  (include "huffman.scm"))
