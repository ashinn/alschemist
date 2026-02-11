
(define-library (chibi video)
  (import (scheme base)
          (scheme file)
          (scheme list)
          (scheme write)
          (srfi 2)
          (srfi 98)
          (srfi 130)
          (chibi assert)
          (chibi json)
          (chibi log)
          (chibi optional)
          (chibi shell))
  (export
   ;; manipulating videos
   clip crop scale fps hflip vflip zoom concat overlay color draw-text
   anull aevalsrc video+audio
   ;; info
   video-source-meta video-source-width video-source-height
   video-source-aspect-ratio
   ;; interface
   play-video write-video ffmpeg-arguments
   ;; parameters
   video-encoder-path video-info-path video-player-path
   video-ref? video-source? video-filter?)
  (include "video.scm"))
