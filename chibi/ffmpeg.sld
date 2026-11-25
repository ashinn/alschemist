(define-library (chibi ffmpeg)
  (import (scheme base)
          (scheme cxr)
          (scheme hash-table)
          (scheme list)
          (scheme vector)
          (scheme write)
          (srfi 98)
          (chibi json)
          (chibi log)
          (chibi optional)
          (chibi shell))
  (export load-media concat split clip video-with-audio
          video audio overlay overlay-text scale crop color
          aevalsrc anull adelay
          ffmpeg-use-longest-stream ffmpeg-preset
          media-file-info ffmpeg-arguments)
  (include "ffmpeg.scm"))
