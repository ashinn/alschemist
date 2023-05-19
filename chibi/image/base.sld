
(define-library (chibi image base)
  (import (scheme base) (scheme bitwise) (scheme list)
          (srfi 160 base) (srfi 179) (chibi assert))
  (export image? make-image image-array image-color-space
          image-height image-width image-channels image-alpha
          pixmap? make-pixmap pixmap-array pixmap-pixel-format
          pixmap-width pixmap-height
          array->image array->pixmap
          image->pixmap pixmap->image)
  (include "base.scm"))
