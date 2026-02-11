
(define-library (chibi video-test)
  (import (scheme base) (chibi video) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "(chibi video)")
      (test '("-ss" "3" "-t" "5" "-i" "input-0.mkv" "-map" "[0]" "-shortest")
          (ffmpeg-arguments
           (clip "input-0.mkv" 3 8)))
      (test '("-ss" "3" "-t" "5" "-i" "input-0.mkv"
              "-ss" "4" "-t" "5" "-i" "input-1.mkv"
              "-filter_complex" "[0] [1] concat [concat]"
              "-map" "[concat]" "-shortest")
          (ffmpeg-arguments
           (concat (clip "input-0.mkv" 3 8) (clip "input-1.mkv" 4 9))))
      (test '("-filter_complex" "color=c=red:s=1280x720:d=5 [color]"
              "-map" "[color]" "-shortest")
          (ffmpeg-arguments (color "red" 1280 720 5)))
      (test '("-filter_complex" "color=c=red:s=1280x720:d=2 [color] ; [color] drawtext=text=hello:x=0:y=0 [drawtext]"
              "-map" "[drawtext]" "-shortest")
          (ffmpeg-arguments (draw-text (color "red" 1280 720 2) "hello")))
      (test '("-filter_complex" "color=c=red:s=1280x720:d=2 [color] ; [color] drawtext=text=hello:x=0:y=0 [drawtext] ; color=c=green:s=1280x720:d=2 [color0] ; [color0] drawtext=text=world:x=0:y=0 [drawtext0] ; [drawtext] [drawtext0] concat [concat]"
              "-map" "[concat]" "-shortest")
          (ffmpeg-arguments
           (concat
            (draw-text (color "red" 1280 720 2) "hello")
            (draw-text (color "green" 1280 720 2) "world"))))
      (test '("-filter_complex" "color=c=red:s=1280x720:d=5 [color] ; anull [anull]"
              "-map" "[color]" "-map" "[anull]"
              "-shortest")
          (ffmpeg-arguments (video+audio (color "red" 1280 720 5)
                                         (anull))))
      (test-error
       (let ((red (color "red" 1280 720 2)))
         (ffmpeg-arguments (concat red red))))
      (test-error
       (let ((v (clip "input-0.mkv" 3 8)))
         (ffmpeg-arguments (concat v v))))
      (test '("-filter_complex"
              "aevalsrc=sin(440*2*PI*t):s=8000:duration=2 [aevalsrc]"
              "-map" "[aevalsrc]" "-shortest")
          (ffmpeg-arguments (aevalsrc 440)))
      (test
          '("-filter_complex" "color=c=red:s=1280x720:d=2 [color] ; aevalsrc=sin(440*2*PI*t):s=8000:duration=2 [aevalsrc]"
            "-map" "[color]" "-map" "[aevalsrc]"
            "-shortest")
          (ffmpeg-arguments
           (video+audio (color "red" 1280 720 2)
                        (aevalsrc 440))))
      (test-end))))
