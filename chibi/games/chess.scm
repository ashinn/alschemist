
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; piece representation

(define piece-none   0) ; 000
(define piece-pawn   1) ; 001
(define piece-knight 2) ; 010
(define piece-king   3) ; 011
(define piece-bishop 5) ; 101
(define piece-rook   6) ; 110
(define piece-queen  7) ; 111

(define (piece-white? piece) (not (bit-set? 3 piece)))
(define (piece-black? piece) (bit-set? 3 piece))
(define (piece-white piece) (bitwise-and piece #b111))
(define (piece-black piece) (bitwise-ior piece #b1000))
(define (piece-player piece) (bitwise-and piece #b1000))
(define (piece-base piece) (bitwise-and piece #b111))
(define (piece-complement piece) (bitwise-xor piece #b1000))
(define piece-diagonal? odd?)
(define (piece-orthogonal-slider? piece) (= #b110 (bitwise-and piece #b110)))
(define (piece-diagonal-slider? piece) (= #b101 (bitwise-and piece #b101)))

;; zero sum

(define (white-piece-value x)
  (vector-ref '#(0 100 300 12800 0 300 500 900
                 0 -100 -300 -12800 0 -300 -500 -900)
              x))

(define (black-piece-value x) (- (white-piece-value x)))

(define black-pawn   (piece-complement piece-pawn))
(define black-knight (piece-complement piece-knight))
(define black-king   (piece-complement piece-king))
(define black-bishop (piece-complement piece-bishop))
(define black-rook   (piece-complement piece-rook))
(define black-queen  (piece-complement piece-queen))

(define (piece-mobility bd index base-piece player)
  (let ((sum 0))
    (define (add! dst p)
      (unless (and (positive? p) (= player (piece-player p)))
        (set! sum (+ sum 1))))
    (cond
     ((eq? base-piece piece-knight)
      (for-each-knight bd index add!))
     ((eq? base-piece piece-bishop)
      (for-each-diagonal* bd player-white index add!))
     ((eq? base-piece piece-rook)
      (for-each-orthogonal* bd player-white index add!))
     ;; tricky - want to discourage queen development
     ((eq? base-piece piece-queen)
      (for-each-diagonal* bd player-white index add!)
      (for-each-orthogonal* bd player-white index add!))
     ;; don't concern with mobility for kings and pawns,
     ;; but consider other heuristics later
     ;; ((eq? base-piece piece-pawn) 0)
     ;; ((eq? base-piece piece-king) 0)
     (else 0))
    sum))

(define (white-piece-mobility bd index piece)
  (if (piece-black? piece)
      (- (piece-mobility bd index (piece-base piece) player-black))
      (piece-mobility bd index piece player-white)))

;; players

(define (player-complement player) (bitwise-xor player #b1000))
(define player-white 0)
(define player-black (player-complement player-white))
(define (player-white? x) (= x player-white))
(define (player-black? x) (= x player-black))

(define player-piece bitwise-ior)

(define (piece->ascii n)
  (case n
    ((1)  #\P)  ((2)  #\N)  ((3)  #\K)  ((5)  #\B)  ((6)  #\R)  ((7)  #\Q)
    ((9)  #\p)  ((10) #\n)  ((11) #\k)  ((13) #\b)  ((14) #\r)  ((15) #\q)
    (else #\.)))

(define (piece->unicode n)
  (case n
    ((3)  #\x2654) ((7)  #\x2655) ((6)  #\x2656) ((5)  #\x2657)
    ((2)  #\x2658) ((1) #\x2659)
    ((11) #\x265A) ((15) #\x265B) ((14) #\x265C) ((13) #\x265D)
    ((10) #\x265E) ((9) #\x265F)
    (else #\.)))

(define (x->base-piece x)
  (cond ((string? x)
         (and (= 1 (string-length x)) (x->base-piece (string-ref x 0))))
        ((char? x)
         (case x
           ((#\k #\K) 3) ((#\q #\Q) 7) ((#\r #\R) 6)
           ((#\b #\B) 5) ((#\n #\N) 2) ((#\p #\P) 1)
           (else #f)))
        ((symbol? x) (x->base-piece (symbol->string x)))
        ((number? x) x)
        (else #f)))

(define (x->piece x . opt)
  (cond
   ((x->base-piece x)
    => (lambda (base-piece)
         (if (and (pair? opt) (player-black? (car opt)))
             (piece-black base-piece)
             base-piece)))
   (else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; moves are fixnums combining the source and destination positions

(define (make-move src dst)
  (+ (arithmetic-shift src 8) dst))

(define (move-source mov)
  (arithmetic-shift mov -8))

(define (move-destination mov)
  (bitwise-and mov #b11111111))

;; loose upper bound on the number of legal moves from any one position
(define (make-move-set . o)
  (let ((len (if (pair? o)
                 (car o)
                 ;; K    Q        R        B        N       P   =  356
                 (+ 8 (* 9 28) (* 2 14) (* 2 14) (* 2 8) (* 8 3)))))
    (make-vector len #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; board representation, 0x88 method, rank/file 0-indexed
;; (using bytevector, u4vector fits but would be slow)

(define (make-chess-board)
  (make-bytevector 128 0))

(define (rank+file->index rank file)
  (+ (arithmetic-shift rank 4) file))

(define (index->rank index)
  (arithmetic-shift index -4))

(define (index->file index)
  (bitwise-and index #b1111))

(define (valid-index? index)
  (zero? (bitwise-and index #x88)))

(define (valid-rank+file? rank file)
  (and (<= 0 rank 7) (<= 0 file 7)))

(define (board-ref bd rank file)
  (bytevector-u8-ref bd (rank+file->index rank file)))

(define (board-set! bd rank file piece)
  (bytevector-u8-set! bd (rank+file->index rank file) piece))

;; additional board info stored in invalid squares

;; unused
;; (define (board-player-to-move bd)
;;   (bytevector-u8-ref bd #b1000))
;; (define (board-player-to-move-set! bd player)
;;   (bytevector-u8-set! bd #b1000 player))

(define (board-en-passant-capturable-file bd)
  (bytevector-u8-ref bd #b1001))
(define (board-en-passant-capturable-file-set! bd file)
  (bytevector-u8-set! bd #b1001 file))

(define kingside 0)
(define queenside 0)

(define (board-castle-disabled? bd player side)
  (= 1 (bytevector-u8-ref bd (+ #b1100 (arithmetic-shift player 1) side))))
(define (board-castle-disable! bd player side)
  (bytevector-u8-set! bd (+ #b1100 (arithmetic-shift player 1) side) 1))

(define (board-no-progress-count bd)
  (bytevector-u8-ref bd #b10001000))
(define (board-no-progress-inc! bd)
  (bytevector-u8-set! bd #b10001000 (+ 1 (bytevector-u8-ref bd #b10001000))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the standard chess layout

(define initial-chess-board
  (let ((bd (make-chess-board)))
    (do ((i 0 (+ i 1))) ((= i 8))
      (board-set! bd 1 i piece-pawn))
    (do ((i 0 (+ i 1))) ((= i 8))
      (board-set! bd 6 i (piece-black piece-pawn)))
    (for-each
     (lambda (rank color)
       (board-set! bd rank 0 (color piece-rook))
       (board-set! bd rank 1 (color piece-knight))
       (board-set! bd rank 2 (color piece-bishop))
       (board-set! bd rank 3 (color piece-queen))
       (board-set! bd rank 4 (color piece-king))
       (board-set! bd rank 5 (color piece-bishop))
       (board-set! bd rank 6 (color piece-knight))
       (board-set! bd rank 7 (color piece-rook)))
     '(0 7) (list piece-white piece-black))
    (board-en-passant-capturable-file-set! bd 8)
    ;; build the layout once and copy it
    (lambda () (bytevector-copy bd))))

(define (print-chess-board bd . opt)
  (define (print port keys)
    (let-keywords* keys
        ((unicode? #f)
         (reverse-video? #f)
         (black? #f)
         (ansi? #f)
         (empty-square (if ansi? " " "."))
         (left #f)
         (center-col #f)
         (right #f)
         (top #f)
         (middle-row #f)
         (bottom #f))
      (if top (write-string top port))
      (do ((rank (if black? 0 7) ((if black? + -) rank 1)))
          ((if black? (> rank 7) (< rank 0)))
        (if (and middle-row (if black? (> rank 0) (< rank 7)))
          (write-string middle-row port))
        (if left (write-string left port))
        (do ((file (if black? 7 0) ((if black? - +) file 1)))
            ((if black? (< file 0) (> file 7)))
          (if (and center-col (if black? (< file 7) (not (zero? file))))
            (write-string center-col port))
          (if ansi?
              (write-string
               (string-append "\x1b[" (if (odd? (+ file rank)) "47" "42") "m")
               port))
          (let ((piece (board-ref bd rank file)))
            (write-string (or (and (= piece piece-none) empty-square)
                         ((if unicode? piece->unicode piece->ascii) piece))
                     port)))
        (if ansi? (write-string " \x1b[0m"))
        (if right (write-string right port))
        (newline port)) 
      (if bottom (write-string bottom port))))
  (if (pair? opt)
    (let ((x (car opt)))
      (cond ((port? x) (print x (cdr opt)))
            ((eq? x #t) (print (current-output-port) (cdr opt)))
            ((eq? x #f) (let ((out (open-output-string)))
                          (print out (cdr opt))
                          (get-output-string out)))
            (else (print (current-output-port) opt))))
    (print (current-output-port) '())))

(define-record-type Chess-Game
  (chess-game board history heuristic move-sets boards)
  chess-game?
  (board chess-game-board chess-game-board-set!)
  (history chess-game-history chess-game-history-set!)
  (heuristic chess-game-heuristic chess-game-heuristic-set!)
  (move-sets chess-game-move-sets chess-game-move-sets-set!)
  (boards chess-game-boards chess-game-boards-set!))

(define (make-chess-game . opt)
  (let ((bd (if (pair? opt) (car opt) (initial-chess-board))))
    (chess-game bd '() (chess-default-heuristic bd) '() '())))

(define (chess-game-apply-simple-move game move)
  (let* ((bd (chess-simple-move (chess-game-board game)
                                (move-source move)
                                (move-destination move)))
         (score (chess-default-heuristic bd)))
    (chess-game bd (cons move (chess-game-history game)) score
                (chess-game-move-sets game)
                (chess-game-boards game))))

(define (chess-game-apply-move game move)
  (let* ((bd (chess-move (chess-game-board game)
                         (move-source move)
                         (move-destination move)))
         (score (chess-default-heuristic bd)))
    (chess-game bd (cons move (chess-game-history game)) score
                (chess-game-move-sets game)
                (chess-game-boards game))))

(define (chess-game-prev game)
  (let ((v (chess-game-history game)))
    (and (pair? v) (car v))))

(define (chess-game-last-player-to-move game)
  (piece-player
   (bytevector-u8-ref (chess-game-board game)
                      (move-destination (chess-game-prev game)))))

(define (chess-game-player-to-move game)
  (player-complement (chess-game-last-player-to-move game)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; describing movement & notation

(define (chess-parse-rank x) ; a number or string representation of a number
  (cond ((number? x) (- x 1))
        ((string? x) (cond ((string->number x) => chess-parse-rank) (else #f)))
        ((char? x)   (chess-parse-rank (string x)))
        ((symbol? x) (chess-parse-rank (symbol->string x)))
        (else #f)))

(define (chess-parse-file x) ; a letter or raw (non-string) number
  (cond ((number? x) (- x 1))
        ((char? x) (- (char->integer (char-downcase x)) (char->integer #\a)))
        ((and (string? x) (= 1 (string-length x)))
         (chess-parse-file (string-ref x 0)))
        ((symbol? x) (chess-parse-file (symbol->string x)))
        (else #f)))

(define (chess-parse-destination x player)
  (cond ((pair? x)
         (let ((rank (chess-parse-rank (cdr x)))
               (file (chess-parse-file (car x))))
           (and rank file (rank+file->index rank file))))
        ((and (string? x) (= (string-length x) 2))
         (chess-parse-destination
          (cons (string-ref x 0) (string-ref x 1)) player))
        (else #f)))

(define (chess-parse-source x bd dest player)
  (cond
    ((chess-parse-destination x player) => values)
    ((string? x)
      (let ((len (string-length x)))
        (case len
          ((1)
           (let ((ls (chess-reverse-moves-by-piece
                      bd dest player (x->piece x player))))
             (and (= 1 (length ls)) (car ls))))
          ((2)
           (let* ((ch (string-ref x 1))
                  (file (chess-parse-file x ch))
                  (rank (chess-parse-rank x ch))
                  (ls (filter (if file
                                (lambda (sq) (= file (cdr sq)))
                                (lambda (sq) (= rank (cdr sq))))
                              (chess-reverse-moves-by-piece
                               bd dest player (x->piece x player)))))
             (and (= 1 (length ls)) (car ls))))
          ((3) (chess-parse-destination (substring x 1 3) player))
          (else #f))))
    (else #f)))

(define (chess-parse-move game move player)
  (cond
   ((pair? move)
    (let* ((dest (chess-parse-destination (cdr move) player))
           (src (chess-parse-source
                 (car move) (chess-game-board game) dest player)))
      (and src dest (make-move src dest))))
   ((string? move)
    (let* ((str move)
           (len (string-length str)))
      (cond
       ((or (string=? str "O-O") (string=? str "o-o"))
        (if (equal? player player-white)
            (chess-parse-move game "e1-g1" player)
            (chess-parse-move game "e8-g8" player)))
       ((or (string=? str "O-O-O") (string=? str "o-o-o"))
        (if (equal? player player-white)
            (chess-parse-move game "e1-c1" player)
            (chess-parse-move game "e8-c8" player)))
       ((string-cursor<? (string-index str (lambda (ch) (memv ch '(#\x #\-))))
                         (string-cursor-end str))
        (let ((i (string-cursor->index
                  str
                  (string-index str (lambda (ch) (memv ch '(#\x #\-)))))))
          (and (<= 1 i (- len 1))
               (chess-parse-move
                game
                (cons (substring str 0 i) (substring str (+ i 1) len))
                player))))
       ((and (= len 2)
             (memv (string-ref str 0) '(#\a #\b #\c #\d #\e #\f #\g #\h))
             (char-numeric? (string-ref str 1)))
        (chess-parse-move game (cons "P" str) player))
       ((= len 3)
        (chess-parse-move game
                          (cons (substring str 0 1) (substring str 1 3))
                          player))
       ((= (string-length str) 4)
        (chess-parse-move game
                          (cons (substring str 0 2) (substring str 2 4))
                          player))
       (else #f))))
   (else
    (error "not a valid move object" move))))

(define (chess-format-move bd move)
  (define (get-rank i)
    (number->string (+ (index->rank i) 1)))
  (define (get-file i)
    (string (integer->char (+ (index->file i) (char->integer #\a)))))
  (let* ((src (move-source move))
         (dest (move-destination move))
         (rank (get-rank dest))
         (file (get-file dest))
         (piece (piece-base (bytevector-u8-ref bd src)))
         (capture? (not (zero? (bytevector-u8-ref bd dest)))))
    (string-append (if (= piece piece-pawn)
                       (if capture? (get-file src) "")
                       (string (piece->ascii piece)))
                   (if capture? "x" "")
                   file
                   rank)))

(define (chess-format-last-move game)
  (chess-format-move (chess-game-board game) (chess-game-prev game)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; movement

(define (chess-simple-move! bd from to)
  (bytevector-u8-set! bd to (bytevector-u8-ref bd from))
  (bytevector-u8-set! bd from piece-none)
  bd)

(define (chess-simple-move bd from to)
  (chess-simple-move! (bytevector-copy bd) from to))

(define (chess-move! bd from to)
  ;; make the move
  (cond
   ((chess-en-passant? bd from to)
    (chess-simple-move! bd from to)
    (bytevector-u8-set! bd ((if (< from to) - +) to 1) piece-none))
   ((if (= 0 (index->rank to))
        (= black-pawn (bytevector-u8-ref bd from))
        (and (= 7 (index->rank to))
             (= piece-pawn (bytevector-u8-ref bd from))))
    ;; TODO: alternate piece promotion
    (bytevector-u8-set! bd from piece-none)
    (bytevector-u8-set! bd to (if (= 0 (index->rank to))
                                  black-queen
                                  piece-queen)))
   ((chess-castle? bd from to)
    (chess-simple-move! bd from to)
    (let ((rook (player-piece (piece-player (bytevector-u8-ref bd to))
                              piece-rook)))
      (case to
        ((6 118)
         (bytevector-u8-set! bd (- to 1) rook)
         (bytevector-u8-set! bd (+ to 1) piece-none))
        ((2 114)
         (bytevector-u8-set! bd (+ to 1) rook)
         (bytevector-u8-set! bd (- to 2) piece-none))
        (else
         (error "invalid castle destination: " to)))))
   (else (chess-simple-move! bd from to)))
  ;; update the board meta info
  (board-en-passant-capturable-file-set!
   bd (if (and (= piece-pawn (piece-base (bytevector-u8-ref bd to)))
               (= 2 (abs (- (index->rank from) (index->rank to)))))
          (index->file from)
          8))
  ;; TODO: update current player info?
  bd)

(define (chess-move bd from to)
  (chess-move! (bytevector-copy bd) from to))

;; slide along a rank, file or diagonal
(define (for-each-slide bd index off proc)
  (let loop ((i (+ index off)))
    (when (valid-index? i)
      (proc i (bytevector-u8-ref bd i))
      (loop (+ i off)))))

;; slide w/o moving through pieces (can capture)
(define (for-each-slide* bd player index off proc)
  (let loop ((i (+ index off)))
    (when (valid-index? i)
      (let ((p (bytevector-u8-ref bd i)))
        (cond ((zero? p) (proc i p) (loop (+ i off)))
              ((not (= player (piece-player p))) (proc i p)))))))

;; explicit list of offsets
(define (for-each-offset bd index ls proc)
  (for-each
   (lambda (off)
     (let ((i (+ index off)))
       (if (valid-index? i) (proc i (bytevector-u8-ref bd i)))))
   ls))

(define (for-each-orthogonal-1 bd index proc)
  (for-each-offset bd index '(-1 +1 -16 +16) proc))

(define (for-each-diagonal-1 bd index proc)
  (for-each-offset bd index '(-15 -17 +15 +17) proc))

(define (for-each-knight bd index proc)
  (for-each-offset
   bd index
   '(-31 -33 -14 -18 +31 +33 +14 +18)
   proc))

(define (for-each-rank bd index proc)
  (for-each-slide bd index -16 proc)
  (for-each-slide bd index +16 proc))

(define (for-each-file bd index proc)
  (for-each-slide bd index -1 proc)
  (for-each-slide bd index +1 proc))

(define (for-each-rank* bd player index proc)
  (for-each-slide* bd player index -16 proc)
  (for-each-slide* bd player index +16 proc))

(define (for-each-file* bd player index proc)
  (for-each-slide* bd player index -1 proc)
  (for-each-slide* bd player index +1 proc))

(define (for-each-orthogonal* bd player index proc)
  (for-each-rank* bd player index proc)
  (for-each-file* bd player index proc))

(define (for-each-diagonal* bd player index proc)
  (for-each-slide* bd player index -15 proc)
  (for-each-slide* bd player index -17 proc)
  (for-each-slide* bd player index +15 proc)
  (for-each-slide* bd player index +17 proc))

(define (board-for-each bd proc)
  (do ((i 0   (+ i 1))) ((= i 8))   (proc i (bytevector-u8-ref bd i)))
  (do ((i 16  (+ i 1))) ((= i 24))  (proc i (bytevector-u8-ref bd i)))
  (do ((i 32  (+ i 1))) ((= i 40))  (proc i (bytevector-u8-ref bd i)))
  (do ((i 48  (+ i 1))) ((= i 56))  (proc i (bytevector-u8-ref bd i)))
  (do ((i 64  (+ i 1))) ((= i 72))  (proc i (bytevector-u8-ref bd i)))
  (do ((i 80  (+ i 1))) ((= i 88))  (proc i (bytevector-u8-ref bd i)))
  (do ((i 96  (+ i 1))) ((= i 104)) (proc i (bytevector-u8-ref bd i)))
  (do ((i 112 (+ i 1))) ((= i 120)) (proc i (bytevector-u8-ref bd i)))
  )

(define-syntax push!
  (syntax-rules ()
    ((push var expr)
     (set! var (cons expr var)))))

;; pieces that can move to a given square
(define (chess-reverse-moves bd index player)
  (let ((dest-piece (bytevector-u8-ref bd index))
        (opponent (player-complement player))
        (white? (player-white? player)))
    (cond
     ((and (not (= piece-none dest-piece))
           (= player (piece-player dest-piece)))
      ;; can't capture our own piece
      '())
     (else
      (let ((res '())
            (my-pawn (x->piece piece-pawn player))
            (my-knight (x->piece piece-knight player))
            (my-king (x->piece piece-king player)))
        ;; pawns
        (when (if white? (>= index 32) (<= index 87))
          (let ((shift (if white? - +)))
            (when (and (not (zero? dest-piece))
                       (not (= player (piece-player dest-piece))))
              ;; pawn captures
              (let ((i (shift index 15)))
                (if (= my-pawn (bytevector-u8-ref bd i)) (push! res i)))
              (let ((i (shift index 17)))
                (if (= my-pawn (bytevector-u8-ref bd i)) (push! res i))))
            (when (zero? dest-piece)    ; pawn moves
              (let* ((i (shift index 16)) (p (bytevector-u8-ref bd i)))
                (if (= my-pawn p)
                    (push! res i)
                    (if (and (zero? p)
                             (if white? (<= 48 index 55) (<= 64 index 71))
                             (= my-pawn
                                (bytevector-u8-ref bd (shift index 32))))
                        (push! res (shift index 32))))))))
        ;; 1-adjacent (king)
        (for-each-orthogonal-1 bd index
          (lambda (i p) (when (= p my-king) (push! res i))))
        (for-each-diagonal-1 bd index
          (lambda (i p) (when (= p my-king) (push! res i))))
        ;; orthogonal (rook, queen)
        (for-each-orthogonal* bd opponent index
          (lambda (i p)
            (when (and (piece-orthogonal-slider? p)
                       (= player (piece-player p)))
              (push! res i))))
        ;; diagonal (bishop, queen)
        (for-each-diagonal* bd opponent index
          (lambda (i p)
            (when (and (piece-diagonal-slider? p) (= player (piece-player p)))
              (push! res i))))
        ;; knight moves
        (for-each-knight bd index
          (lambda (i p) (when (= p my-knight) (push! res i))))
        ;; return
        res)))))

(define (chess-reverse-moves-by-piece bd index player piece)
  (filter (lambda (i) (and (number? piece) (= piece (bytevector-u8-ref bd i))))
          (chess-reverse-moves bd index player)))

(define (chess-piece-moves! bd index move-set i)
  (let* ((piece (bytevector-u8-ref bd index))
         (player (piece-player piece))
         (base (piece-base piece)))
    (define (add! dst p)
      (vector-set! move-set i (make-move index dst))
      (set! i (+ i 1)))
    (define (safe-add! dst p)
      (when (or (zero? p) (not (= player (piece-player p))))
        (add! dst p)))
    (cond
     ((eq? base piece-pawn)
      (let ((shift (if (piece-white? piece) + -)))
        ;; movement
        (when (zero? (bytevector-u8-ref bd (shift index 16)))
          (add! (shift index 16) #f)
          (when (and (if (piece-white? piece)
                         (<= 16 index 23)
                         (<= 96 index 103))
                     (zero? (bytevector-u8-ref bd (shift index 32))))
            (add! (shift index 32) #f)))
        ;; capture
        (let ((p (bytevector-u8-ref bd (shift index 15))))
          (when (and (not (zero? p)) (not (= player (piece-player p))))
            (add! (shift index 15) #f)))
        (let ((p (bytevector-u8-ref bd (shift index 17))))
          (when (and (not (zero? p)) (not (= player (piece-player p))))
            (add! (shift index 17) #f)))
        ;; en passant
        (set! i (chess-en-passant-moves! bd index move-set i))))
     ((eq? base piece-knight) (for-each-knight bd index safe-add!))
     ((eq? base piece-bishop) (for-each-diagonal* bd player index add!))
     ((eq? base piece-rook) (for-each-orthogonal* bd player index add!))
     ((eq? base piece-queen)
      (for-each-orthogonal* bd player index add!)
      (for-each-diagonal* bd player index add!))
     ((eq? base piece-king)
      ;; adjacent
      (for-each-orthogonal-1 bd index safe-add!)
      (for-each-diagonal-1 bd index safe-add!)
      ;; castling
      (set! i (chess-castle-moves! bd index move-set i))))
    i))

(define (chess-piece-moves->list bd index)
  (let* ((ms (make-move-set 28))
         (i (chess-piece-moves! bd index ms 0)))
    (vector->list ms 0 i)))

;; returns true iff src->dest on bd would be capturing en passant
(define (chess-en-passant? bd src dest)
  (and (not (= (index->file src) (index->file dest)))
       (= piece-pawn (piece-base (bytevector-u8-ref bd src)))
       (= piece-none (bytevector-u8-ref bd dest))))

;; insert the (at most 1) en passant move for the piece at the given index
(define (chess-en-passant-moves! bd index move-set i)
  (let ((player (piece-player (bytevector-u8-ref bd index)))
        (file (board-en-passant-capturable-file bd)))
    (cond
     ((>= file 8)
      i)
     ((and (player-white? player)
           (> file 0)
           (= piece-pawn (board-ref bd 4 (- file 1))))
      (vector-set! move-set i (make-move (rank+file->index 4 (- file 1))
                                         (rank+file->index 5 file)))
      (+ i 1))
     ((and (player-white? player)
           (< file 7)
           (= piece-pawn (board-ref bd 4 (+ file 1))))
      (vector-set! move-set i (make-move (rank+file->index 4 (+ file 1))
                                         (rank+file->index 5 file)))
      (+ i 1))
     ((and (player-black? player)
           (> file 0)
           (= black-pawn (board-ref bd 3 (- file 1))))
      (vector-set! move-set i (make-move (rank+file->index 3 (- file 1))
                                         (rank+file->index 2 file)))
      (+ i 1))
     ((and (player-black? player)
           (< file 7)
           (= black-pawn (board-ref bd 3 (+ file 1))))
      (vector-set! move-set i (make-move (rank+file->index 3 (+ file 1))
                                         (rank+file->index 2 file)))
      (+ i 1))
     (else
      i))))

;; returns true iff src->dest on bd would be castling
(define (chess-castle? bd src dest)
  (and (= 2 (abs (- dest src)))
       (= piece-king (piece-base (bytevector-u8-ref bd src)))))

(define (chess-castle-moves! bd index move-set i)
  (cond
   ((and (= index 4) (= piece-king (bytevector-u8-ref bd 4)))
    (when (and (zero? (bytevector-u8-ref bd 6))
               (zero? (bytevector-u8-ref bd 5))
               (= piece-rook (bytevector-u8-ref bd 7))
               (not (board-castle-disabled? bd player-white kingside))
               (null? (chess-reverse-moves bd 4 player-white))
               (null? (chess-reverse-moves bd 5 player-white)))
      (vector-set! move-set i (make-move 4 6))
      (set! i (+ i 1)))
    (when (and (zero? (bytevector-u8-ref bd 1))
               (zero? (bytevector-u8-ref bd 2))
               (zero? (bytevector-u8-ref bd 3))
               (= piece-rook (bytevector-u8-ref bd 0))
               (not (board-castle-disabled? bd player-white queenside))
               (null? (chess-reverse-moves bd 4 player-white))
               (null? (chess-reverse-moves bd 3 player-white)))
      (vector-set! move-set i (make-move 4 2))
      (set! i (+ i 1))))
   ((and (= index 116) (= black-king (bytevector-u8-ref bd 116)))
    (when (and (zero? (bytevector-u8-ref bd 118))
               (zero? (bytevector-u8-ref bd 117))
               (= black-rook (bytevector-u8-ref bd 119))
               (not (board-castle-disabled? bd player-black kingside))
               (null? (chess-reverse-moves bd 116 player-black))
               (null? (chess-reverse-moves bd 117 player-black)))
      (vector-set! move-set i (make-move 116 118))
      (set! i (+ i 1)))
    (when (and (zero? (bytevector-u8-ref bd 113))
               (zero? (bytevector-u8-ref bd 114))
               (zero? (bytevector-u8-ref bd 115))
               (= black-rook (bytevector-u8-ref bd 112))
               (not (board-castle-disabled? bd player-black queenside))
               (null? (chess-reverse-moves bd 116 player-black))
               (null? (chess-reverse-moves bd 115 player-black)))
      (vector-set! move-set i (make-move 116 114))
      (set! i (+ i 1)))))
  i)

(define (chess-board-moves! bd player move-set)
  (let ((offset 0))
    (board-for-each bd
     (lambda (i p)
       (when (and (not (zero? p )) (= player (piece-player p)))
         (set! offset (chess-piece-moves! bd i move-set offset)))))
    offset))

(define (chess-board-moves->list bd player . o)
  (let* ((move-set (if (pair? o) (car o) (make-move-set)))
         (i (chess-board-moves! bd player move-set)))
    (vector->list move-set 0 i)))

(define (chess-find-king bd player)
  (let ((my-king (player-piece player piece-king)))
    (call-with-current-continuation
     (lambda (return)
       (board-for-each bd (lambda (i p) (if (= p my-king) (return i))))
       #f))))

(define (board-checking-pieces bd defending-player)
  (let ((index (chess-find-king bd defending-player)))
    (if index
        (chess-reverse-moves bd index (player-complement defending-player))
        '())))

(define (board-check? bd defending-player)
  (pair? (board-checking-pieces bd defending-player)))

(define (chess-check? game . o)
  (let ((defending-player
          (if (pair? o) (car o) (chess-game-player-to-move game))))
    (pair? (board-checking-pieces (chess-game-board game) defending-player))))

(define (chess-mate? game)
  ;; Check if we're in check and still in check after any move.  Note
  ;; we record the current defending player for the next-ply checks
  ;; since the player will have changed.
  (and (chess-check? game)
       (let ((bd (chess-game-board game))
             (defending-player (chess-game-player-to-move game)))
         (every (lambda (game) (chess-check? game defending-player))
                (map (lambda (move) (chess-game-apply-move game move))
                     (chess-board-moves->list
                      bd
                      (chess-game-last-player-to-move game)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; searching

;; TODO: quiescience

;; material values only
(define (chess-material-heuristic bd)
  (let lp ((bd bd) (i 0) (lim 8) (res 0))
    (cond
     ((= i lim)
      (if (= lim 120) res (lp bd (+ i 8) (+ lim 16) res)))
     (else
      (lp bd (+ i 1) lim
          (+ res (white-piece-value (bytevector-u8-ref bd i))))))))

;; encourage development
(define (chess-default-heuristic bd)
  (let lp ((bd bd) (i 0) (lim 8) (res 0))
    (cond
     ((= i lim)
      (if (= lim 120) res (lp bd (+ i 8) (+ lim 16) res)))
     (else
      (let ((piece (bytevector-u8-ref bd i)))
        (if (eq? piece piece-none)
            (lp bd (+ i 1) lim res)
            (lp bd (+ i 1) lim
                (+ res
                   (white-piece-value piece)
                   (white-piece-mobility bd i piece)))))))))

;; Of all my possible moves, choose the best response my opponent can
;; give, and keep track of which move is best for me.  Return just the
;; value of the best move.

(define (minimax bd player depth alpha beta move-sets boards)
  (if (zero? depth)
      (chess-default-heuristic bd)
      (let* ((move-set (car move-sets))
             (move-sets (cdr move-sets))
             (bd2 (car boards))
             (boards (cdr boards))
             (n (chess-board-moves! bd player move-set)))
        (let loop ((i 0) (alpha alpha) (beta beta))
          (cond
           ((>= i n)
            (if (player-white? player) alpha beta))
           (else
            (bytevector-copy! bd2 0 bd)
            (chess-move! bd2
                         (move-source (vector-ref move-set i))
                         (move-destination (vector-ref move-set i)))
            (let ((riposte (minimax bd2
                                    (player-complement player)
                                    (- depth 1)
                                    alpha
                                    beta
                                    move-sets
                                    boards)))
              (if (if (player-white? player)
                      (> riposte alpha)
                      (< riposte beta))
                  (if (player-white? player)
                      (if (>= riposte beta)
                          beta
                          (loop (+ i 1) riposte beta))
                      (if (<= riposte alpha)
                          alpha
                          (loop (+ i 1) alpha riposte)))
                  (loop (+ i 1) alpha beta)))))))))

;; The entry point search, like minimax above but we track and return
;; which move gave the best value.

(define (chess-choose-move game player . o)
  (let ((depth (if (pair? o) (car o) 3))
        (better? (if (player-white? player) > <)))
    (when (< (length (chess-game-move-sets game)) depth)
      (chess-game-move-sets-set! game (map (lambda _ (make-move-set)) (iota depth)))
      (chess-game-boards-set! game (map (lambda _ (make-chess-board)) (iota depth))))
    (let* ((move-set (car (chess-game-move-sets game)))
           (move-sets (cdr (chess-game-move-sets game)))
           (bd (chess-game-board game))
           (bd2 (car (chess-game-boards game)))
           (boards (cdr (chess-game-boards game)))
           (n (chess-board-moves! bd player move-set)))
      (let loop ((i 0) (best #f) (best-move #f))
        (cond
         ((>= i n)
          best-move)
         (else
          (bytevector-copy! bd2 0 bd)
          (chess-move! bd2
                       (move-source (vector-ref move-set i))
                       (move-destination (vector-ref move-set i)))
          (if (board-check? bd2 player)
              ;; skip - can't move into check
              (loop (+ i 1) best best-move)
              (let ((riposte (minimax bd2
                                      (player-complement player)
                                      (- depth 1)
                                      -20000
                                      +20000
                                      move-sets
                                      boards)))
                ;;(write `(,(vector-ref move-set i) => ,riposte) (current-error-port)) (newline (current-error-port))
                (if (and riposte (or (not best) (better? riposte best)))
                    (loop (+ i 1) riposte (vector-ref move-set i))
                    (loop (+ i 1) best best-move))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *invalid-move-syntax*
  "  Invalid move: I only understand algebraic notation (e.g. e4, Nf3, etc.): ")

(define *illegal-move*
  "  Illegal move: ")

(define (main . args)
  (let ((unicode? #f)  ; "unicode|u"
        (reverse-video? #f)  ; "reverse-video|rv"
        (ansi? #f)  ; "ansi|a"
        (black? #f)  ; "black|b"
        (no-display? #f))  ; "n|no-display"
    (let* ((player (if black? player-black player-white))
           (computer (player-complement player))
           (move-set (make-move-set)))
      (define (prompt-move player i)
        (write-string (if (player-white? player) "\nWhite(" "\nBlack("))
        (write-string (number->string i))
        (write-string "): ")
        (flush-output-port))
      (define (player-move game i)
        (let loop ()
          (prompt-move player i)
          (let ((resp (read-line)))
            (when (or (eof-object? resp) (member resp '("exit" "quit")))
              (exit 0))
            (let ((move (chess-parse-move game resp player)))
              (cond
               ((member move
                        (chess-board-moves->list
                         (chess-game-board game) player move-set))
                (chess-game-apply-move game move))
               (else
                (write-string (if move  ;(chess-notation-valid? resp)
                                  *illegal-move*
                                  *invalid-move-syntax*))
                (write-string resp)
                (newline)
                (write (map (lambda (move)
                              (chess-format-move (chess-game-board game) move))
                            (chess-board-moves->list
                             (chess-game-board game) player move-set)))
                (newline)
                (loop)))))))
      (define (computer-move game i)
        (let ((new (chess-choose-move game computer)))
          (cond
           (new
            (prompt-move computer i)
            (write-string
             (chess-format-move (chess-game-board game) new))
            (newline)
            (chess-game-apply-move game new))
           (else
            (write-string "Stalemate.  It's a draw.\n")
            (exit 0)))))
      (define (redisplay game)
        (unless no-display?
          (print-chess-board
           (chess-game-board game)
           ':center-col " " ':top "\n" ':bottom "\n" ':left "  "
           ':unicode? unicode? ':ansi? ansi? ':black? black?
           ':reverse-video? reverse-video?)))
      (let ((order (if (player-white? player)
                       (list player-move computer-move)
                       (list computer-move player-move)))
            (start (make-chess-game)))
        (unless black? (redisplay start))
        (let turn ((game start) (i 1) (o order))
          (if (null? o)
              (turn game (+ i 1) order)
              (let ((new ((car o) game i)))
                (redisplay new)
                ;;(write `(heuristic: ,(chess-default-heuristic (chess-game-board game)))) (newline)
                (cond
                 ((chess-mate? new)
                  (write-string "Checkmate!  ")
                  (write-string (if (eq? (car o) player-move) "You" "I"))
                  (write-string " win!\n")
                  (exit 0))
                 (else
                  (turn new i (cdr o)))))))))))

;; Local Variables:
;; eval: (put 'for-each-orthogonal-1 'scheme-indent-function 2)
;; eval: (put 'for-each-diagonal-1 'scheme-indent-function 2)
;; eval: (put 'for-each-orthogonal* 'scheme-indent-function 3)
;; eval: (put 'for-each-diagonal* 'scheme-indent-function 3)
;; eval: (put 'for-each-knight 'scheme-indent-function 2)
;; eval: (put 'board-for-each 'scheme-indent-function 1)
;; End:
