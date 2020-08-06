;;> Minimal xlib bindings.  Names are converted from CamelCase to
;;> camel-case, with the "X" prefix dropped.

;;> Example:
;;> \schemeblock{
;;> ;; Connect and open a small black window.
;;> (define dsp (open-display ":0"))
;;> (define screen (default-screen dsp))
;;> (define rootwin (root-window dsp screen))
;;> (define cmap (default-colormap dsp screen))
;;> (define white (white-pixel dsp screen))
;;> (define black (black-pixel dsp screen))
;;> (define win (create-simple-window dsp rootwin 0 0 200 200 1 black black))
;;> (define gc1 (create-gc dsp win 0))
;;> (set-foreground dsp gc1 white)
;;> (define gc2 (create-gc dsp win 0))
;;> (set-foreground dsp gc2 black)
;;> (define font (load-query-font dsp "*-helvetica-*-12-*"))
;;> (set-font dsp gc1 (font-id font))
;;> (select-input dsp win (+ KeyPressMask ButtonPressMask ExposureMask))
;;> (map-window dsp win)
;;> (flush dsp)
;;> ;; Loop, displaying each keystroke until a button is clicked.
;;> (define e (make-event))
;;> (let lp ()
;;>  (next-event dsp e)
;;>  (cond
;;>   ((eq? (event-type e) Expose)
;;>    (draw-string dsp win gc 10 10 "Hello World!")
;;>    (lp))
;;>   ((eq? (event-type e) KeyPress)
;;>    (let* ((code (key-event-keycode (event-key e)))
;;>           (state (key-event-state (event-key e)))
;;>           (str (string-append "key: " (number->string code)
;;>                               " state: " (number->string state))))
;;>      (fill-rectangle dsp win gc2 0 11 200 30)
;;>      (draw-string dsp win gc 10 30 str))
;;>    (lp))
;;>   ((not (eq? (event-type e) ButtonPress))
;;>    (lp))))
;;> }

(define-library (chibi xlib)
  (import (chibi))
  (include-shared "xlib")
  (export
   display? graphics-context? font?
   open-display root-window default-screen default-colormap
   black-pixel white-pixel store-name create-simple-window
   create-gc set-foreground select-input map-window flush
   load-query-font set-font font-id
   draw-string draw-line draw-point draw-rectangle draw-arc fill-rectangle 
   ;; XEvent union
   make-event event? event-type next-event put-back-event
   event-any event-key event-button event-motion event-expose
   any-event-serial any-event-send-event any-event-display any-event-window
   expose-event-x expose-event-y
   expose-event-width expose-event-height expose-event-count
   key-event-x key-event-y key-event-time key-event-state key-event-keycode
   button-event-x button-event-y button-event-time
   button-event-state button-event-button
   motion-event-x motion-event-y motion-event-time motion-event-state
   ;; constants
   NoEventMask KeyPressMask KeyReleaseMask ButtonPressMask ButtonRelease
   ExposureMask EnterWindowMask LeaveWindowMask
   KeyPress KeyRelease ButtonPress ButtonRelease
   MotionNotify EnterNotify LeaveNotify FocusIn FocusOut KeymapNotify
   Expose GraphicsExpose NoExpose
   ))
