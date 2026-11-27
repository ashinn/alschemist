
(define-library (chibi ffmpeg-test)
  (import (scheme base) (chibi ffmpeg) (chibi test))
  (export run-tests)
  (begin
    (define (run-tests)
      (test-begin "(chibi ffmpeg)")
      (test-group "inputs"
        (test '()
            (ffmpeg-arguments (video-with-audio (audio (color 'red)) (video (anull)))))
        (test '("-i" "input-0.mkv" "-map" "0:v" "-map" "0:a" "-shortest")
            (ffmpeg-arguments (load-media "input-0.mkv")))
        (test '("-i" "input-0.mkv" "-map" "0:v" "-shortest")
            (ffmpeg-arguments (video (load-media "input-0.mkv"))))
        (test '("-i" "input-0.mkv" "-map" "0:v" "-shortest")
            (parameterize ((ffmpeg-preset "bogus"))
              (ffmpeg-arguments (video (load-media "input-0.mkv")))))
        (test '("-i" "input-0.mkv" "-map" "0:a" "-preset" "fast")
            (parameterize ((ffmpeg-use-longest-stream #t)
                           (ffmpeg-preset "fast"))
              (ffmpeg-arguments (audio (load-media "input-0.mkv")))))
        (test '("-i" "input-0.mkv" "-i" "input-1.mkv" "-map" "0:v" "-map" "1:a" "-shortest")
            (ffmpeg-arguments
             (video-with-audio (load-media "input-0.mkv") (load-media "input-1.mkv"))))
        (test "only reachable inputs are used"
            '("-i" "input-1.mkv" "-map" "0:v" "-map" "0:a" "-shortest")
          (let ((unused (load-media "input-0.mkv")))
            (ffmpeg-arguments (load-media "input-1.mkv"))))
        (test "same file shares an input"
            '("-i" "input-0.mkv" "-map" "0:v" "-map" "0:a" "-shortest")
          (ffmpeg-arguments
           (video-with-audio (load-media "input-0.mkv") (load-media "input-0.mkv")))))
      (test-group "clip"
        (test '("-i" "input-0.mkv" "-filter_complex"
                "[0:v] trim=start=3:end=8 [v1]; [v1] setpts=PTS-STARTPTS [v2]; [0:a] atrim=start=3:end=8 [a3]; [a3] asetpts=PTS-STARTPTS [a4]"
                "-map" "[v2]" "-map" "[a4]" "-shortest")
            (ffmpeg-arguments (load-media "input-0.mkv" 3 8)))
        (test '("-i" "input-0.mkv" "-filter_complex"
                "[0:v] trim=start=1.5 [v1]; [v1] setpts=PTS-STARTPTS [v2]"
                "-map" "[v2]" "-shortest")
            (ffmpeg-arguments (clip (video (load-media "input-0.mkv")) 3/2))))
      (test-group "concat"
        (test '("-i" "input-0.mkv" "-i" "input-1.mkv" "-filter_complex"
                "[0:v] trim=start=3:end=8 [v1]; [v1] setpts=PTS-STARTPTS [v2]; [0:a] atrim=start=3:end=8 [a3]; [a3] asetpts=PTS-STARTPTS [a4]; [1:v] trim=start=4:end=9 [v5]; [v5] setpts=PTS-STARTPTS [v6]; [1:a] atrim=start=4:end=9 [a7]; [a7] asetpts=PTS-STARTPTS [a8]; [v2][a4][v6][a8] concat=n=2:v=1:a=1 [v10][a9]"
                "-map" "[v10]" "-map" "[a9]" "-shortest")
            (ffmpeg-arguments
             (concat (load-media "input-0.mkv" 3 8)
                     (load-media "input-1.mkv" 4 9))))
        (test '("-filter_complex"
                "color=c=red:s=1280x720:d=2 [src1]; [src1] drawtext=text=hello:expansion=none:x=10:y=10:fontsize=24:fontcolor=white [v2]; color=c=green:s=1280x720:d=2 [src3]; [src3] drawtext=text=world:expansion=none:x=10:y=10:fontsize=24:fontcolor=white [v4]; [v2][v4] concat=n=2:v=1:a=0 [v5]"
                "-map" "[v5]" "-shortest")
            (ffmpeg-arguments
             (concat
              (overlay-text (color 'red 2) "hello")
              (overlay-text (color "green" 2) "world"))))
        (test "unused outputs go to a null sink"
            '("-i" "input-0.mkv" "-filter_complex"
              "[0:v][0:a] concat=n=1:v=1:a=1 [v2][a1]; [a1] anullsink"
              "-map" "[v2]" "-shortest")
          (ffmpeg-arguments (video (concat (load-media "input-0.mkv")))))
        (test-error (concat))
        (test-error (concat (load-media "input-0.mkv") (color 'red 2))))
      (test-group "automatic splits"
        (test '("-filter_complex"
                "color=c=red:s=1280x720:d=2 [src1]; [src1] split=2 [sp2][sp3]; [sp2][sp3] concat=n=2:v=1:a=0 [v4]"
                "-map" "[v4]" "-shortest")
            (let ((red (color 'red 2)))
              (ffmpeg-arguments (concat red red))))
        (test '("-filter_complex"
                "color=c=red:s=1280x720:d=2 [src1]; [src1] split=3 [sp2][sp3][sp4]; [sp2][sp3][sp4] concat=n=3:v=1:a=0 [v5]"
                "-map" "[v5]" "-shortest")
            (ffmpeg-arguments (apply concat (split (color 'red 2) 3))))
        (test '("-i" "input-0.mkv" "-filter_complex"
                "[0:v] split=2 [sp1][sp2]; [0:a] asplit=2 [sp3][sp4]; [sp1][sp3][sp2][sp4] concat=n=2:v=1:a=1 [v6][a5]"
                "-map" "[v6]" "-map" "[a5]" "-shortest")
            (let ((v (load-media "input-0.mkv")))
              (ffmpeg-arguments (concat v v))))
        (test "a stream used both in the graph and as output"
            '("-filter_complex"
              "color=c=red:s=1280x720:d=2 [src1]; [src1] split=2 [sp2][sp3]; [sp2] scale=w=320:h=180 [v4]; [sp3][v4] overlay=x=0:y=0 [v5]"
              "-map" "[v5]" "-shortest")
          (let ((red (color 'red 2)))
            (ffmpeg-arguments (overlay red (scale red 320 180))))))
      (test-group "sources"
        (test '("-filter_complex" "color=c=red:s=1280x720:d=5 [src1]"
                "-map" "[src1]" "-shortest")
            (ffmpeg-arguments (color 'red 5)))
        (test '("-filter_complex" "color=c=red:s=320x240 [src1]"
                "-map" "[src1]" "-shortest")
            (ffmpeg-arguments (color 'red #f "320x240")))
        (test '("-filter_complex"
                "aevalsrc=exprs=sin(440*2*PI*t):s=8000:d=2 [src1]"
                "-map" "[src1]" "-shortest")
            (ffmpeg-arguments (aevalsrc 440 2 8000)))
        (test '("-filter_complex"
                "aevalsrc=exprs=if(lt(t\\,1)\\,0\\,sin(440*2*PI*t)):s=44100 [src1]"
                "-map" "[src1]" "-shortest")
            (ffmpeg-arguments (aevalsrc "if(lt(t,1),0,sin(440*2*PI*t))")))
        (test '("-filter_complex"
                "color=c=red:s=1280x720:d=5 [src1]; anullsrc=r=44100:d=5 [src2]"
                "-map" "[src1]" "-map" "[src2]" "-shortest")
            (ffmpeg-arguments (video-with-audio (color 'red 5) (anull 5))))
        (test '("-filter_complex"
                "color=c=red:s=1280x720:d=2 [src1]; aevalsrc=exprs=sin(440*2*PI*t):s=44100 [src2]"
                "-map" "[src1]" "-map" "[src2]" "-shortest")
            (ffmpeg-arguments
             (video-with-audio (color 'red 2) (aevalsrc 440)))))
      (test-group "filters"
        (test '("-i" "input-0.mkv" "-filter_complex"
                "[0:v] scale=w=640:h=360 [v1]; [v1] crop=w=320:h=180:x=10:y=20 [v2]"
                "-map" "[v2]" "-map" "0:a" "-shortest")
            (ffmpeg-arguments
             (crop (scale (load-media "input-0.mkv") 640 360) 320 180 10 20)))
        (test "scale ignores audio-only media"
            '("-filter_complex" "anullsrc=r=44100 [src1]" "-map" "[src1]" "-shortest")
          (ffmpeg-arguments (scale (anull) 640 360)))
        (test "overlay-text escapes"
            '("-filter_complex"
              "color=c=red:s=1280x720 [src1]; [src1] drawtext=text=it\\\\\\'s 50%\\\\: a\\,b\\;\\[c\\]:expansion=none:x=0:y=0:fontsize=32:fontcolor=yellow [v2]"
              "-map" "[v2]" "-shortest")
          (ffmpeg-arguments
           (overlay-text (color 'red) "it's 50%: a,b;[c]" 0 0 32 'yellow)))
        (test '("-i" "input-0.mkv" "-filter_complex"
                "[0:a] adelay=250:all=1 [a1]"
                "-map" "0:v" "-map" "[a1]" "-shortest")
            (ffmpeg-arguments (adelay (load-media "input-0.mkv") 250))))
      (test-group "overlay"
        (test '("-i" "input-0.mkv" "-i" "input-1.mkv" "-filter_complex"
                "[0:v][1:v] overlay=x=10:y=20 [v1]; [0:a][1:a] amix=inputs=2:duration=first:dropout_transition=0 [a2]"
                "-map" "[v1]" "-map" "[a2]" "-shortest")
            (ffmpeg-arguments
             (overlay (load-media "input-0.mkv") (load-media "input-1.mkv") 10 20)))
        (test "delayed overlay"
            '("-i" "input-0.mkv" "-i" "input-1.mkv" "-filter_complex"
              "[1:v] setpts=PTS-STARTPTS+1.5/TB [v1]; [0:v][v1] overlay=x=0:y=0:enable='gte(t,1.5)' [v2]; [1:a] adelay=1500:all=1 [a3]; [0:a][a3] amix=inputs=2:duration=first:dropout_transition=0 [a4]"
              "-map" "[v2]" "-map" "[a4]" "-shortest")
          (ffmpeg-arguments
           (overlay (load-media "input-0.mkv") (load-media "input-1.mkv") 0 0 1.5)))
        (test "audio-only foreground is mixed in"
            '("-i" "input-0.mkv" "-i" "input-1.mkv" "-filter_complex"
              "[0:v] trim=start=3:end=8 [v1]; [v1] setpts=PTS-STARTPTS [v2]; [0:a] atrim=start=3:end=8 [a3]; [a3] asetpts=PTS-STARTPTS [a4]; [1:v] trim=start=4:end=9 [v5]; [v5] setpts=PTS-STARTPTS [v6]; [1:a] atrim=start=4:end=9 [a7]; [a7] asetpts=PTS-STARTPTS [a8]; [v2][a4][v6][a8] concat=n=2:v=1:a=1 [v10][a9]; aevalsrc=exprs=sin(440*2*PI*t):s=44100 [src11]; [a9][src11] amix=inputs=2:duration=first:dropout_transition=0 [a12]"
              "-map" "[v10]" "-map" "[a12]" "-shortest")
          (ffmpeg-arguments
           (overlay (concat (load-media "input-0.mkv" 3 8) (load-media "input-1.mkv" 4 9))
                    (aevalsrc 440))))
        (test "audio-only background takes the foreground video"
            '("-filter_complex"
              "color=c=red:s=1280x720 [src1]; anullsrc=r=44100 [src2]"
              "-map" "[src1]" "-map" "[src2]" "-shortest")
          (ffmpeg-arguments (overlay (anull) (color 'red)))))
      (test-end))))
