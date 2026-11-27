
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define assq-ref
  (opt-lambda (ls key (default #f))
    (cond ((assq key ls) => cdr) (else default))))

(define (->number x)
  (if (string? x)
      (string->number x)
      x))

(define assq-ref/number
  (opt-lambda (ls key (default 0))
    (cond ((assq key ls) => (lambda (cell) (->number (cdr cell))))
          (else default))))

(define (string-join lst delimiter)
  (if (null? lst) ""
      (let loop ((tail (cdr lst)) (acc (car lst)))
        (if (null? tail) acc
            (loop (cdr tail) (string-append acc delimiter (car tail)))))))

(define ffmpeg-use-longest-stream
  (make-parameter #f))

(define ffmpeg-preset
  (make-parameter (get-environment-variable "CHIBI_FFMPEG_PRESET")))

(define ffmpeg-valid-presets
  '("ultrafast" "superfast" "veryfast" "faster" "fast"
    "medium" "slow" "slower" "veryslow" "placebo"))

(define (find-font name)
  ;; TODO
  "/usr/share/fonts/truetype/ubuntu/Ubuntu[wdth,wght].ttf")

(define media-encoder-path (make-parameter "ffmpeg"))

(define media-info-path (make-parameter "ffprobe"))

(define media-player-command
  ;; TODO: fallback if missing to '(ffplay -autoexit)
  (make-parameter '(mpv --no-terminal)))

(define-syntax shell->json
  (syntax-rules ()
    ((shell->json cmd ...)
     (let ((str (shell->string cmd ...)))
       (log-trace "shell command: " `(cmd ...))
       (log-trace "json output: " str)
       (string->json str)))))

(define-record-type Media-Ref
  (make-media-ref kind node index)
  media-ref?
  (kind media-ref-kind media-ref-kind-set!)
  (node media-ref-node media-ref-node-set!)
  (index media-ref-index media-ref-index-set!))

(define-record-type Media
  (make-media video audio)
  media?
  (video media-video media-video-set!)
  (audio media-audio media-audio-set!))

(define-record-type Input-Node
  (make-input-node filename)
  input-node?
  (filename input-node-filename input-node-filename-set!))

(define-record-type Source-Node
  (make-source-node kind filter-name args)
  source-node?
  (kind source-node-kind source-node-kind-set!)
  (filter-name source-node-filter-name source-node-filter-name-set!)
  (args source-node-args source-node-args-set!))

(define-record-type Filter-Node
  (make-filter-node filter-name args inputs out-v-count out-a-count)
  filter-node?
  (filter-name filter-node-filter-name filter-node-filter-name-set!)
  (args filter-node-args filter-node-args-set!)
  (inputs filter-node-inputs filter-node-inputs-set!)
  (out-v-count filter-node-out-v-count filter-node-out-v-count-set!)
  (out-a-count filter-node-out-a-count filter-node-out-a-count-set!))

(define (node-inputs node)
  (cond
   ((filter-node? node) (filter-node-inputs node))
   ((or (source-node? node) (input-node? node)) '())
   (else (error "not a node" node))))

;; Formats a filter option value: symbols and strings as is, inexact
;; or non-integral numbers as decimals.
(define (arg->string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((and (exact? x) (not (integer? x))) (number->string (inexact x)))
        (else (number->string x))))

;; Escapes str for use as a filter option value inside a filtergraph,
;; which is parsed twice: once by the filtergraph parser (which treats
;; [],;'\ specially), then again by the option parser (':'\).
(define (escape-filter-arg str)
  (define (escape specials str)
    (let ((out (open-output-string)))
      (string-for-each
       (lambda (ch)
         (if (memv ch specials) (write-char #\\ out))
         (write-char ch out))
       str)
      (get-output-string out)))
  (escape '(#\\ #\' #\[ #\] #\, #\;)
          (escape '(#\\ #\' #\:) str)))

;; Returns a new ref to the single output of the filter name applied
;; to ref, preserving its kind.
(define (filter-ref ref name args)
  (let ((video? (eq? 'video (media-ref-kind ref))))
    (make-media-ref (media-ref-kind ref)
                    (make-filter-node name args (list ref)
                                      (if video? 1 0)
                                      (if video? 0 1))
                    0)))

(define (map-video media proc)
  (if (media-video media)
      (make-media (proc (media-video media)) (media-audio media))
      media))

(define (map-audio media proc)
  (if (media-audio media)
      (make-media (media-video media) (proc (media-audio media)))
      media))

;; Loads the default video and audio streams from the file at path,
;; optionally clipped to the time range [start, end) in seconds.
(define-opt (load-media path (start 0) (end #f))
  (let* ((node (make-input-node path))
         (media (make-media (make-media-ref 'video node 0)
                            (make-media-ref 'audio node 0))))
    (if (or (positive? start) end)
        (clip media start end)
        media)))

;; Projections.
(define (video media) (make-media (media-video media) #f))
(define (audio media) (make-media #f (media-audio media)))
(define (video-with-audio v a) (make-media (media-video v) (media-audio a)))

;; Returns a list of n medias equivalent to media.  This is only for
;; readability: streams used more than once are split automatically
;; by ffmpeg-arguments.
(define-opt (split media (n 2))
  (make-list n media))

;; Clips media to the time range [start, end) in seconds, resetting
;; the timestamps to start at 0.
(define-opt (clip media start (end #f))
  (let ((media (if (string? media) (load-media media) media))
        (args (string-append
               "start=" (arg->string start)
               (if end (string-append ":end=" (arg->string end)) ""))))
    (map-audio
     (map-video media
                (lambda (v)
                  (filter-ref (filter-ref v "trim" args)
                              "setpts"
                              "PTS-STARTPTS")))
     (lambda (a)
       (filter-ref (filter-ref a "atrim" args) "asetpts" "PTS-STARTPTS")))))

(define (scale media width height)
  (map-video media
             (lambda (v)
               (filter-ref v "scale" (string-append
                                      "w=" (arg->string width)
                                      ":h=" (arg->string height))))))

(define-opt (crop media width height (x 0) (y 0))
  (map-video media
             (lambda (v)
               (filter-ref v "crop" (string-append "w=" (arg->string width)
                                                   ":h=" (arg->string height)
                                                   ":x=" (arg->string x)
                                                   ":y=" (arg->string y))))))

;; Draws the literal text (no % expansion) on the video.
(define-opt (overlay-text media text
                          (x 10) (y 10) (font-size 24) (font-color 'white))
  (map-video
   media
   (lambda (v)
     (filter-ref v "drawtext"
                 (string-append "text=" (escape-filter-arg text)
                                ":expansion=none"
                                ":x=" (arg->string x)
                                ":y=" (arg->string y)
                                ":fontsize=" (arg->string font-size)
                                ":fontcolor=" (arg->string font-color))))))

;; A solid color video, infinite unless duration (in seconds) is given.
(define-opt (color color (duration #f) (size "1280x720"))
  (let* ((args (string-append
                "c=" (arg->string color) ":s=" (arg->string size)
                (if duration (string-append ":d=" (arg->string duration)) "")))
         (node (make-source-node 'video "color" args)))
    (make-media (make-media-ref 'video node 0) #f)))

;; Audio generated from expr, an ffmpeg expression in t, or if a
;; number, a sine wave of that frequency in Hz.
(define-opt (aevalsrc expr (duration #f) (sample-rate 44100))
  (let* ((expr (if (number? expr)
                   (string-append "sin(" (arg->string expr) "*2*PI*t)")
                   expr))
         (args (string-append
                "exprs=" (escape-filter-arg expr)
                ":s=" (arg->string sample-rate)
                (if duration (string-append ":d=" (arg->string duration)) "")))
         (node (make-source-node 'audio "aevalsrc" args)))
    (make-media #f (make-media-ref 'audio node 0))))

;; Silent audio, infinite unless duration (in seconds) is given.
(define-opt (anull (duration #f) (sample-rate 44100))
  (let* ((args (string-append
                "r=" (arg->string sample-rate)
                (if duration (string-append ":d=" (arg->string duration)) "")))
         (node (make-source-node 'audio "anullsrc" args)))
    (make-media #f (make-media-ref 'audio node 0))))

;; Concatenates the medias in sequence.  Either all or none of the
;; medias must have video, and likewise for audio.
(define (concat . medias)
  (if (null? medias)
      (error "concat requires at least one media"))
  (let ((has-video? (any media-video medias))
        (has-audio? (any media-audio medias)))
    (if (or (and has-video? (not (every media-video medias)))
            (and has-audio? (not (every media-audio medias))))
        (error "concat: medias must all have the same streams, use video, audio or video-with-audio to make them match"
               (map (lambda (m) (list (and (media-video m) 'video)
                                      (and (media-audio m) 'audio)))
                    medias)))
    (let* ((video-count (if has-video? 1 0))
           (audio-count (if has-audio? 1 0))
           (inputs (append-map
                    (lambda (media)
                      (filter values (list (media-video media)
                                           (media-audio media))))
                    medias))
           (args (string-append "n=" (number->string (length medias))
                                ":v=" (number->string video-count)
                                ":a=" (number->string audio-count)))
           (node (make-filter-node "concat" args inputs video-count audio-count)))
      (make-media (and has-video? (make-media-ref 'video node 0))
                  (and has-audio? (make-media-ref 'audio node 0))))))

;; Delays the audio by delay-millis milliseconds.
(define (adelay media delay-millis)
  (map-audio media
             (lambda (a)
               (filter-ref a "adelay"
                           (string-append (arg->string delay-millis) ":all=1")))))

;; Contextually blends bg and fg, with fg starting at start-time
;; seconds into bg:
;; - If both have video: overlays fg's video at (x, y).
;; - If both have audio: mixes the audio, for the duration of bg.
;; - Otherwise uses whichever stream is present, preferring bg.
(define-opt (overlay bg fg (x 0) (y 0) (start-time 0))
  (let ((out-v
         (if (and (media-video bg) (media-video fg))
             (let ((fg-video
                    (if (positive? start-time)
                        (filter-ref (media-video fg) "setpts"
                                    (string-append "PTS-STARTPTS+"
                                                   (arg->string start-time)
                                                   "/TB"))
                        (media-video fg))))
               (make-media-ref
                'video
                (make-filter-node
                 "overlay"
                 (string-append "x=" (arg->string x)
                                ":y=" (arg->string y)
                                (if (positive? start-time)
                                    (string-append ":enable='gte(t,"
                                                   (arg->string start-time)
                                                   ")'")
                                    ""))
                 (list (media-video bg) fg-video)
                 1 0)
                0))
             (or (media-video bg) (media-video fg))))
        (out-a
         (if (and (media-audio bg) (media-audio fg))
             (let ((fg-audio
                    (if (positive? start-time)
                        (media-audio (adelay (audio fg)
                                             (exact (round (* start-time 1000)))))
                        (media-audio fg))))
               (make-media-ref
                'audio
                (make-filter-node "amix"
                                  "inputs=2:duration=first:dropout_transition=0"
                                  (list (media-audio bg) fg-audio)
                                  0 1)
                0))
             (or (media-audio bg) (media-audio fg)))))
    (make-media out-v out-a)))

;; Two refs denote the same stream if they name the same output pad
;; of the same node, even if they are distinct records.
(define (media-ref=? a b)
  (and (eq? (media-ref-node a) (media-ref-node b))
       (eq? (media-ref-kind a) (media-ref-kind b))
       (= (media-ref-index a) (media-ref-index b))))

(define (media-ref-cell ref alist)
  (find (lambda (cell) (media-ref=? ref (car cell))) alist))

;; Returns all nodes reachable from the list of media-refs, with each
;; node appearing after all of the nodes it takes input from.
(define (toposort refs)
  (define (visit ref res)
    (let ((node (media-ref-node ref)))
      (if (memq node res)
          res
          (cons node (fold visit res (node-inputs node))))))
  (reverse (fold visit '() refs)))

(define (filter-spec name args)
  (let ((name (if (symbol? name) (symbol->string name) name)))
    (if (and args (not (equal? args "")))
        (string-append name "=" args)
        name)))

;; Compiles the filter graph producing media into a list of ffmpeg
;; command-line arguments (inputs, -filter_complex and -map), not
;; including the ffmpeg command itself or any output options.  Only
;; input files reachable from media are included, and streams
;; consumed more than once are automatically split.
(define (ffmpeg-arguments media)
  (let ((terminals (filter values (list (media-video media) (media-audio media)))))
    (if (null? terminals)
        '()
        (let* ((nodes (toposort terminals))
               (files (delete-duplicates
                       (filter-map (lambda (node)
                                     (and (input-node? node)
                                          (input-node-filename node)))
                                   nodes)))
               (counts '())   ; ((ref . number-of-uses) ...)
               (labels '())   ; ((ref . unconsumed-labels) ...)
               (filters '())  ; reversed filter specs
               (counter 0))
          (define (use-count ref)
            (cond ((media-ref-cell ref counts) => cdr) (else 0)))
          (define (count-use! ref)
            (cond ((media-ref-cell ref counts)
                   => (lambda (cell) (set-cdr! cell (+ (cdr cell) 1))))
                  (else (set! counts (cons (cons ref 1) counts)))))
          (define (new-label prefix)
            (set! counter (+ counter 1))
            (string-append "[" prefix (number->string counter) "]"))
          (define (emit! . strs)
            (set! filters (cons (string-join strs " ") filters)))
          (define (video? ref) (eq? 'video (media-ref-kind ref)))
          ;; Register label as the output of ref, splitting it into
          ;; one label per use if needed.  Unused filter outputs are
          ;; sent to a null sink since ffmpeg rejects dangling pads.
          (define (provide! ref label from-input?)
            (let ((n (use-count ref)))
              (cond
               ((> n 1)
                (let ((outs (map (lambda (i) (new-label "sp")) (iota n))))
                  (emit! label
                         (string-append (if (video? ref) "split=" "asplit=")
                                        (number->string n))
                         (string-join outs ""))
                  (set! labels (cons (cons ref outs) labels))))
               ((= n 1)
                (set! labels (cons (cons ref (list label)) labels)))
               ((not from-input?)
                (emit! label (if (video? ref) "nullsink" "anullsink"))))))
          (define (consume! ref)
            (let ((cell (media-ref-cell ref labels)))
              (if (not (and cell (pair? (cdr cell))))
                  (error "unresolved media stream" ref))
              (let ((label (cadr cell)))
                (set-cdr! cell (cddr cell))
                label)))
          ;; Count every consumer of each stream, including the final -map.
          (for-each (lambda (node) (for-each count-use! (node-inputs node)))
                    nodes)
          (for-each count-use! terminals)
          ;; Generate the filters in dependency order.
          (for-each
           (lambda (node)
             (cond
              ((input-node? node)
               (let ((idx (number->string
                           (list-index (lambda (f) (equal? f (input-node-filename node)))
                                       files))))
                 (for-each
                  (lambda (cell)
                    (let ((ref (car cell)))
                      (when (eq? node (media-ref-node ref))
                        (provide! ref
                                  (string-append
                                   "[" idx (if (video? ref) ":v" ":a")
                                   (if (zero? (media-ref-index ref))
                                       ""
                                       (string-append
                                        ":" (number->string (media-ref-index ref))))
                                   "]")
                                  #t))))
                  (reverse counts))))
              ((source-node? node)
               (let ((label (new-label "src")))
                 (emit! (filter-spec (source-node-filter-name node)
                                     (source-node-args node))
                        label)
                 (provide! (make-media-ref (source-node-kind node) node 0)
                           label
                           #f)))
              (else
               (let* ((ins (map consume! (filter-node-inputs node)))
                      (outs
                       (append
                        (map (lambda (i)
                               (cons (make-media-ref 'video node i) (new-label "v")))
                             (iota (filter-node-out-v-count node)))
                        (map (lambda (i)
                               (cons (make-media-ref 'audio node i) (new-label "a")))
                             (iota (filter-node-out-a-count node))))))
                 (emit! (string-join ins "")
                        (filter-spec (filter-node-filter-name node)
                                     (filter-node-args node))
                        (string-join (map cdr outs) ""))
                 (for-each (lambda (out) (provide! (car out) (cdr out) #f))
                           outs)))))
           nodes)
          ;; Map the final streams.  Unsplit input streams are mapped
          ;; directly by stream specifier, without brackets.
          (let ((maps (map (lambda (ref)
                             (let ((label (consume! ref)))
                               (if (and (input-node? (media-ref-node ref))
                                        (= 1 (use-count ref)))
                                   (substring label 1 (- (string-length label) 1))
                                   label)))
                           terminals)))
            `(,@(append-map (lambda (file) (list "-i" file)) files)
              ,@(if (pair? filters)
                    (list "-filter_complex" (string-join (reverse filters) "; "))
                    '())
              ,@(append-map (lambda (label) (list "-map" label)) maps)
              ,@(if (ffmpeg-use-longest-stream) '() '("-shortest"))
              ,@(if (member (ffmpeg-preset) ffmpeg-valid-presets)
                    (list "-preset" (ffmpeg-preset))
                    '())))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry points

(define (write-video vid output-file)
  (shell (,(media-encoder-path)
          -y
          ,@(ffmpeg-arguments vid)
          ,output-file)))

(define (play-video vid)
  ;; First pass to ffmpeg to handle complex filters, and stream the
  ;; output to ffplay.
  (let ((args (ffmpeg-arguments vid)))
    (log-debug `(,(media-encoder-path) ,@args -f matroska -))
    (shell (,(media-encoder-path) ,@args -f matroska -)
           (,(media-player-path) -))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspecting videos

(define (media-file-meta path)
  (shell->json
   (,(media-info-path)
    -of json
    -show_format
    -show_streams
    -v quiet
    ,path)))

;; Simplified version that just extracts and parses the most relevant
;; parts of the fulle metadata.
(define (media-file-info x)
  (let* ((meta (media-file-meta x))
         (streams (assq-ref meta 'streams))
         (video-stream
          (find (lambda (s) (equal? "video" (assq-ref s 'codec_type)))
                (if (vector? streams)
                    (vector->list streams)
                    '()))))
    (cond
     ((not (vector? streams))
      `(video))
     (video-stream
      `(video
        (width . ,(assq-ref/number video-stream 'width))
        (height . ,(assq-ref/number video-stream 'height))
        (fps . ,(assq-ref/number video-stream 'r_frame_rate))
        (duration . ,(assq-ref/number video-stream 'duration))))
     (else
      `(audio
        (duration . ,(assq-ref/number (vector-ref streams 0) 'duration)))))))
