
;; A fully functional video definition language.  You can compose an
;; arbitrary DAG of audio and video sources loaded from files, or
;; filters on those videos, and the compositions return placeholders.
;; To generate the video we then walk the DAG via the placeholders to
;; construct the necessary ffmpeg command.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

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

(define video-encoder-path (make-parameter "ffmpeg"))

(define video-info-path (make-parameter "ffprobe"))

(define video-player-path (make-parameter "mpv")) ;; better than ffplay

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities

(define (hhmmss->seconds str)
  (let ((colon0 (string-index str #\:))
        (end (string-cursor-end str)))
    (if (string-cursor<? colon0 end)
        (let* ((mstart (string-cursor-next str colon0))
               (colon1 (string-index str #\: mstart)))
          (let ((hh (string->number (substring/cursors str 0 colon0)))
                (mm (string->number (substring/cursors str mstart colon1)))
                (ss (if (string-cursor<? colon1 end)
                        (string->number
                         (substring/cursors
                          str (string-cursor-next str colon1) end))
                        0)))
            (+ (* 60 60 hh) (* 60 mm) ss)))
        (string->number str))))

(define (hhmmss x)
  (if (string? x)
      (hhmmss->seconds x)
      x))

(define (display-to-string x)
  (if (string? x)
      x
      (let ((out (open-output-string)))
        (display x out)
        (let ((res (get-output-string out)))
          (close-output-port out)
          res))))

(define assq-ref
  (opt-lambda (ls key (default #f))
    (cond ((assq key ls) => cdr) (else default))))

(define-syntax if-splice
  (syntax-rules ()
    ((if-splice test expr)
     (if test expr '()))))

(define-syntax shell->json
  (syntax-rules ()
    ((shell->json cmd ...)
     (let ((str (shell->string cmd ...)))
       (log-trace "shell command: " `(cmd ...))
       (log-trace "json output: " str)
       (string->json str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(define-record-type Video-Source
  (make-video-source file start end meta)
  video-source?
  ;; A string representing the path containing the video source.
  (file video-source-file video-source-file-set!)
  ;; The start offset within the video to load.
  (start video-source-start video-source-start-set!)
  ;; The end offset within the video to load.
  (end video-source-end video-source-end-set!)
  (meta %video-source-meta %video-source-meta-set!))

(define (video-source-duration source)
  (and (video-source-end source)
       (- (video-source-end source)
          (or (video-source-start source) 0))))

(define-record-type Video-Filter
  (make-video-filter name inputs options outputs)
  video-filter?
  ;; A symbol representing the ffmpeg filter name.
  (name video-filter-name video-filter-name-set!)
  ;; A possibly empty list of Video inputs to the filter.
  (inputs video-filter-inputs video-filter-inputs-set!)
  ;; An alist of options to the filter.
  (options video-filter-options video-filter-options-set!)
  ;; A possibly empty list of output string names.
  (outputs video-filter-outputs video-filter-outputs-set!))

(define video-filter
  (opt-lambda* (name
                (inputs '())
                (options '())
                (outputs (list (symbol->string name))))
    (let* ((inputs (if (list? inputs) inputs (list inputs)))
           (vf (make-video-filter name inputs options outputs))
           (f-outputs (map (lambda (i) (make-video-ref vf i))
                           (iota (length outputs)))))
      (apply values f-outputs))))

;; The output of any video-source or video-filter is one or more
;; labels, which can each be used subsequently in other combinators.
;; Each source filter should be processed once.  The labels can be
;; used at most one time - any additional uses require a split filter.
(define-record-type Video-Ref
  (%make-video-ref source offset audio-source audio-offset)
  video-ref?
  ;; The source this video came from.
  (source video-ref-source video-ref-source-set!)
  ;; The index within the output values of the source,
  ;; typically 0 for sources or only one output.
  (offset video-ref-offset video-ref-offset-set!)
  ;; If specified, map the audio from this separate source.
  (audio-source video-ref-audio-source video-ref-audio-source-set!)
  (audio-offset video-ref-audio-offset video-ref-audio-offset-set!))

(define make-video-ref
  (opt-lambda (source (offset 0) (audio-source #f) (audio-offset 0))
    (%make-video-ref source offset audio-source audio-offset)))

(define-record-type Vstate
  (%make-vstate inputs filters labels names)
  vstate?
  ;; A reversed list of arguments loading input sources to ffmpeg.
  (inputs vstate-inputs vstate-inputs-set!)
  ;; A reversed list of complex filters.
  (filters vstate-filters vstate-filters-set!)
  (labels vstate-labels vstate-labels-set!)
  ;; (("name" . count) ...)
  (names vstate-names vstate-names-set!))

(define (make-vstate)
  (%make-vstate '() '() '() '()))

(define (copy-vstate state)
  (%make-vstate (vstate-inputs state)
                (vstate-filters state)
                (vstate-labels state)
                (vstate-names state)))

(define (vstate-with-inputs state inputs)
  (%make-vstate inputs
                (vstate-filters state)
                (vstate-labels state)
                (vstate-names state)))

(define (vstate-push-input state input)
  (vstate-with-inputs state (cons input (vstate-inputs state))))

(define (vstate-with-labels state labels)
  (%make-vstate (vstate-inputs state)
                (vstate-filters state)
                labels
                (vstate-names state)))

(define (vstate-push-label state label)
  (assert (and (pair? label)
               (or (video-source? (car label)) (video-filter? (car label)))))
  (assert (every (lambda (x) (and (pair? x) (integer? (car x)))) (cdr label)))
  (vstate-with-labels state (cons label (vstate-labels state))))

(define (vstate-replace-label state label)
  (assert (and (pair? label)
               (or (video-source? (car label)) (video-filter? (car label)))))
  (assert (every (lambda (x) (and (pair? x) (integer? (car x)))) (cdr label)))
  (vstate-with-labels state
                      (cons label (remove (lambda (ols)
                                            (eq? (car label) (car ols)))
                                          (vstate-labels state)))))

(define (vstate-with-filters state filters)
  (%make-vstate (vstate-inputs state)
                filters
                (vstate-labels state)
                (vstate-names state)))

(define (vstate-push-filter state vf)
  (assert (video-filter? vf))
  (vstate-with-filters state (cons vf (vstate-filters state))))

(define (vstate-with-names state names)
  (assert (list? names))
  (assert (every pair? names))
  (%make-vstate (vstate-inputs state)
                (vstate-filters state)
                (vstate-labels state)
                names))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry points

(define (write-video vid output-file)
  (shell (,(video-encoder-path)
          -y
          ,@(ffmpeg-arguments vid)
          ,output-file)))

(define (play-video vid)
  ;; First pass to ffmpeg to handle complex filters, and stream the
  ;; output to ffplay.
  (let ((args (ffmpeg-arguments vid)))
    (log-debug `(,(video-encoder-path) ,@args -f matroska -))
    (shell (,(video-encoder-path) ,@args -f matroska -)
           (,(video-player-path) -))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

;;> Load a video clip from a file, optionally truncating to the given
;;> start/end seconds.
(define clip
  (opt-lambda (file (start #f) (end #f))
    (make-video-ref
     (make-video-source file (and start (hhmmss start)) (and end (hhmmss end)) #f)
     0)))

;;> Creates a solid color frame for the given duration.
(define (color color-name width height duration)
  (video-filter
   'color
   '()
   `((c . ,color-name)
     (s . ,(string-append (number->string width) "x" (number->string height)))
     (d . ,duration))))

(define (draw-text vid text . o)
  (let-keywords* o ((x 0) (y 0)
                    (font-file #f) (font-size #f) (font-color #f)
                    (box #f) (box-color "black")
                    (shadow-color #f) (shadow-x 3)(shadow-y 3)
                    (time-code #f) (text-file #f))
    (video-filter
     'drawtext
     vid
     `((text . ,text) ;; or load text-file
       (x . ,x)
       (y . ,y)
       ,@(if-splice font-file `((fontfile . ,(find-font font-file))))
       ,@(if-splice font-size `((fontsize . ,font-size)))
       ,@(if-splice font-color `((fontcolor . ,font-color)))
       ,@(if-splice box `((box . 1) (boxcolor . ,box-color)))
       ,@(if-splice shadow-color
                    `((shadowcolor . ,shadow-color)
                      (shadowx . ,shadow-x)
                      (shadowy . ,shadow-y)))
       ,@(if-splice time-code `((time-code . 1)))))))

;;> Concatenate the video arguments in order.
(define (concat . vids)
  (video-filter 'concat vids '()))

;;> Crop the video to the given width and height, starting from the
;;> given x, y coordinate (default top-left).  Leave the height as #f
;;> to crop with the same aspect ratio.
(define crop
  (opt-lambda*
   (vid
    (width (video-source-width vid))
    (height (exact (round (/ width (video-source-aspect-ratio vid)))))
    (x 0)
    (y 0))
   (let ((args (list width height x y)))
     (video-filter 'crop vid args))))

;;> Scale the video to the given width and height.
(define (scale vid . o)
  (opt-lambda*
   (vid
    (width (video-source-width vid))
    (height (exact (round (/ width (video-source-aspect-ratio vid))))))
   (let ((args (list width height)))
     (make-video-filter 'scale vid args))))

;;> Set the frames-per-second.  Rate can be a real number or one of
;;> the constants: source_fps, ntsc, pal, film, ntsc_film.
(define (fps vid rate)
  (video-filter 'fps vid (list rate)))

;;> Flip the video horizontally.
(define (hflip vid . o)
  (video-filter 'hflip vid o))

;;> Flip the video vertically.
(define (vflip vid . o)
  (video-filter 'vflip vid o))

(define (zoom vid . o)
  (video-filter 'zoom vid o))

(define (overlay . vids)
  (video-filter 'overlay vids))

(define (video+audio vid audio)
  (if (video-ref-audio-source audio)
      (make-video-ref (video-ref-source vid)
                      (video-ref-offset vid)
                      (video-ref-audio-source audio)
                      (video-ref-audio-offset audio))
      (make-video-ref (video-ref-source vid)
                      (video-ref-offset vid)
                      (video-ref-source audio)
                      (video-ref-offset audio))))

(define (anull)
  (video-filter 'anull))

(define (aevalsrc frequency . o)
  (let-keywords* o ((sample-rate 8000)
                    (duration 2))
    (video-filter 'aevalsrc
                  '()
                  `(,(string-append "sin("
                                    (number->string frequency)
                                    "*2*PI*t)")
                    (s . ,sample-rate)
                    (duration . ,duration)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internals

(define (video-source-index video state)
  (cond
   ((list-index (lambda (x) (eq? x video)) (vstate-inputs state))
    => (lambda (index) (- (length (vstate-inputs state)) index 1)))
   (else 0)))

;; An alist of labels: (vid (offset . {video-label audio-label used?}) ...)
;; vid can be a video source or filter.
;; offset is the output offset for the video filter, or 0 for a source.
;; The labels are strings, and may be numbers (e.g. "1") indicating
;; the numbered input source, or labels generated by the filters.
(define (resolve-video-ref ref state)
  (let ((vf (video-ref-source ref))
        (offset (video-ref-offset ref))
        (labels (vstate-labels state)))
    (cond
     ((video-source? vf)
      (if (assq vf labels)
          ;; TODO: implicit split
          (error "multiple uses of input" vf)
          (let ((label (number->string (video-source-index vf state))))
            (values label
                    (vstate-push-label state `(,vf (,offset . ,label)))))))
     ((assq vf labels)
      => (lambda (vf-labels)
           (cond
            ((assq offset (cdr vf-labels))
             => (lambda (cell)
                  ;; TODO: implicit split
                  (error "multiple uses of filter" vf)))
            (else
             ;; This offset wasn't used yet.
             (let*-values
                 (((name) (list-ref (video-filter-outputs vf) offset))
                  ((state unique-name) (vstate-unique-name state name)))
               (values unique-name
                       (vstate-replace-label
                        state
                        `(,vf (,offset . ,unique-name)
                              ,@vf-labels))))))))
     (else
      ;; This filter wasn't labeled yet.
      (let*-values (((name)
                     (list-ref (video-filter-outputs vf) offset))
                    ((state unique-name) (vstate-unique-name state name)))
        (values unique-name
                (vstate-push-label state `(,vf (,offset . ,unique-name)))))))))

;; As above but only performs lookup, assuming already walked.
(define (lookup-video-ref ref state)
  (let ((vf (video-ref-source ref))
        (offset (video-ref-offset ref)))
    (or (cond
         ((video-source? vf)
          (number->string (video-source-index vf state)))
         ((assq vf (vstate-labels state))
          => (lambda (vf-labels)
               (cond
                ((assq offset (cdr vf-labels)) => cdr)
                (else #f))))
         (else #f))
        (video-label-name vf))))

(define (video-label-name x)
  (cond
   ((string? x) x)
   ((symbol? x) (symbol->string x))
   ((video-ref? x)
    (string-append (video-label-name (video-ref-source x))
                   (number->string (video-ref-offset x))))
   ((video-filter? x) (video-label-name (video-filter-name x)))
   (else (error "can't get label name for " x))))

(define (vstate-unique-name state name)
  (cond
   ((assoc name (vstate-names state))
    => (lambda (cell)
         (values (vstate-with-names
                  state
                  (cons (cons name (+ 1 (cdr cell)))
                        (remove (lambda (cell) (equal? name (car cell)))
                                (vstate-names state))))
                 (string-append name (number->string (cdr cell))))))
   (else
    (values (vstate-with-names
             state
             (cons (cons name 0) (vstate-names state)))
            name))))

(define (video-source-args source)
  `(,(video-source-file source) "-i"
    ,@(if-splice (and (video-source-duration source)
                      (> (video-source-duration source) 0))
                 `(,(number->string (video-source-duration source)) "-t"))
    ,@(if-splice (and (video-source-start source)
               (> (video-source-start source) 0))
          `(,(number->string (video-source-start source)) "-ss"))))

(define (video-filter-args video state)
  (define (fmt-filter-opts opts)
    (cond
     ((string? opts)
      opts)
     ((pair? opts)
      (string-join
       (map (lambda (x)
              (if (pair? x)
                  (string-append (display-to-string (car x))
                                 "="
                                 (display-to-string (cdr x)))
                  (display-to-string x)))
            opts)
       ":"))
     (else
      "")))
  (define (format-filter-opts filter opts)
    (let ((res (fmt-filter-opts opts)))
      (if (equal? res "")
          filter
          (string-append filter "=" res))))
  (string-append
   (string-join
    (map (lambda (x)
           (let ((label (if (video-ref? x)
                            (lookup-video-ref x state)
                            (video-label-name x))))
             (string-append "[" label "] ")))
         (video-filter-inputs video))
    "")
   (format-filter-opts (symbol->string (video-filter-name video))
                       (video-filter-options video))
   (string-join
    (map (lambda (x) (string-append " [" (if (video-ref? x)
                                         (lookup-video-ref x state)
                                         (video-label-name x))
                                "]"))
         (map (lambda (i) (make-video-ref video i))
              (iota (length (video-filter-outputs video)))))
    "")))

(define (ffmpeg-walk video state)
  (cond
   ((assq video (vstate-labels state))
    ;; already processed
    state)
   ((video-source? video)
    ;; update the input count and use that label
    (let* ((count (length (vstate-inputs state)))
           (label (number->string count))
           (state2 (copy-vstate state)))
      (vstate-inputs-set! state2 `(,video ,@(vstate-inputs state2)))
      state2))
   ((video-filter? video)
    ;; walk the inputs and update the filters and output count
    ;; TODO: can use each channel at most once (need to track),
    ;; after which an implicit split is required.
    (let* ((state2
            (fold (lambda (input state)
                    (if (video-ref? input)
                        (let*-values
                            ;; resolve once on input to ensure
                            ;; that the ref is used once
                            (((state)
                              (ffmpeg-walk (video-ref-source input) state))
                             ((off-label state)
                              (resolve-video-ref input state)))
                          (ffmpeg-walk input state))
                        (ffmpeg-walk input state)))
                  state
                  (video-filter-inputs video)))
           (state3 (copy-vstate state2)))
      (vstate-filters-set! state3 `(,video ,@(vstate-filters state3)))
      state3))
   ((video-ref? video)
    (let ((state (ffmpeg-walk (video-ref-source video) state)))
      (if (and (video-ref-audio-source video)
               (not (eq? (video-ref-audio-source video)
                         (video-ref-source video))))
          (ffmpeg-walk (video-ref-audio-source video) state)
          state)))
   (else
    (error "can't walk non-video" video))))

(define (ffmpeg-arguments video)
  (let*-values (((state) (ffmpeg-walk video (make-vstate)))
                ((label state)
                 (cond
                  ((video-ref? video)
                   (resolve-video-ref video state))
                  (else
                   (values (video-label-name video) state)))))
    `(,@(reverse (append-map video-source-args (vstate-inputs state)))
      ,@(if-splice
         (pair? (vstate-filters state))
         `("-filter_complex"
           ,(string-join
             (reverse (map (lambda (vf) (video-filter-args vf state))
                           (vstate-filters state)))
             " ; ")))
      "-map" ,(string-append "[" label "]")
      ,@(if-splice
         (and (video-ref? video) (video-ref-audio-source video))
         `("-map" ,(string-append
                    "["
                    (lookup-video-ref
                     (make-video-ref (video-ref-audio-source video)
                                     (video-ref-audio-offset video))
                     state)
                    "]")))
      ,@(if-splice (not (ffmpeg-use-longest-stream))
                   '("-shortest"))
      ,@(if-splice (member (ffmpeg-preset) ffmpeg-valid-presets)
                   `("-preset" ,(ffmpeg-preset))))))

(define (video-origin vid)
  (cond
   ((video-source? vid)
    (video-source-file vid))
   ((video-filter? vid)
    (any video-origin (video-filter-inputs vid)))
   ((video-ref? vid)
    (video-origin (video-ref-source vid)))
   (else
    (error "not a video source" vid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inspecting videos

(define (video-source-meta vid)
  (if (video-ref? vid)
      (video-source-meta (video-ref-source vid))
      (or (%video-source-meta vid)
          (let ((res (shell->json
                      (,(video-info-path)
                       -of json
                       -show_format
                       -show_streams
                       -v quiet
                       ,(video-origin vid)))))
            (if (video-source? vid)
                (%video-source-meta-set! vid res))
            res))))

(define (video-source-meta/stream-0 vid)
  (let ((streams (assq-ref (video-source-meta vid) 'streams)))
    (and (vector? streams)
         (positive? (vector-length streams))
         (vector-ref streams 0))))

(define (video-source-width vid)
  (and-let* ((stream-0 (video-source-meta/stream-0 vid)))
    (assq-ref stream-0 'width)))

(define (video-source-height vid)
  (and-let* ((stream-0 (video-source-meta/stream-0 vid)))
    (assq-ref stream-0 'height)))

(define (video-source-aspect-ratio vid)
  (/ (video-source-width vid) (video-source-height vid)))

;; (define (video-end vid)
;;   (or (%video-end vid)
;;       (cond
;;        ((pair? (video-source vid))
;;         (if (equal? "concat" (video-filter vid))
;;             (video-end (last (video-source vid)))
;;             (fold max 0 (map video-end (video-source vid)))))
;;        ((video? (video-source vid))
;;         (video-end (video-source vid)))
;;        ((not (video-source vid))
;;         (assq-ref (video-filter-args vid) 'd 0))
;;        (else
;;         (let* ((json (video-meta (video-source vid)))
;;                (format (assq 'format json))
;;                (duration (assq 'duration (if format (cdr format) '())))
;;                (end (and duration (string->number (cdr duration)))))
;;           (cond
;;            (end
;;             (%video-end-set! vid end)
;;             end)
;;            (else
;;             (error "couldn't determine duration" vid))))))))

;; (define (video-duration vid)
;;   (- (video-end vid) (video-start vid)))
