(define-library (chibi games chess-test)
  (export run-tests)
  (import (scheme base) (srfi 1) (chibi string) (chibi test) (chibi games chess))
  (begin
    ;; TODO: X-FEN support
    (define (parse-board str)
      (define (piece ch)
        (case ch
          ((#\k) 11) ((#\q) 15) ((#\r) 14)
          ((#\b) 13) ((#\n) 10) ((#\p) 9)
          ((#\K) 3) ((#\Q) 7) ((#\R) 6)
          ((#\B) 5) ((#\N) 2) ((#\P) 1)
          (else 0)))
      (let ((bd (make-chess-board)))
        (let lp ((i 7)
                 (rows (map string-trim (string-split str #\newline))))
          (if (null? rows)
              bd
              (do ((j 0 (+ j 1)))
                  ((= j 8) (lp (- i 1) (cdr rows)))
                (board-set! bd i j (piece (string-ref (car rows) j))))))))
    (define (test-move-white expect depth str)
      (let* ((bd (parse-board str))
             (game (make-chess-game bd)))
        (test expect
            (chess-format-move bd (chess-choose-move game player-white depth)))))
    (define (test-move-black expect depth str)
      (let* ((bd (parse-board str))
             (game (make-chess-game bd)))
        (test expect
            (chess-format-move bd (chess-choose-move game player-black depth)))))
    (define (run-tests)
      (test-begin "chess")
      (let* ((bd (parse-board
                  "....k..r
                   pp...ppp
                   ..p.....
                   ...pP...
                   ........
                   .....N..
                   PPP..PPP
                   R...K..R"))
             (game (make-chess-game bd)))
        (test (make-move 4 6)
            (chess-parse-move game "O-O" player-white)))
      (let ((game (make-chess-game)))
        (test-not (chess-check? game))
        (test-not (chess-mate? game)))
      (let ((game (chess-game-from-moves '("f4" "e5" "g4" "Qh4"))))
        (test-assert (chess-check? game))
        (test-assert (chess-mate? game)))
      (let ((game (chess-game-from-moves '("f4" "e5" "e4" "Qh4"))))
        (test-assert (chess-check? game))
        (test-not (chess-mate? game)))
      (let ((game (chess-game-from-moves
                   '("e4" "e6" "d4" "d5" "ed" "ed" "Bb5+"))))
        (test-assert (chess-check? game))
        (test-not (chess-mate? game)))
      (let ((game (chess-game-from-moves
                   '("e4"  "d5"   "ed" "Qxd5"  "Nc3" "Qe5"
                     "Be2" "Nf6"  "Nf3"  "Qe6"   "d4"  "Qc6"
                     "Bb5" "e5"   "Bxc6" "Nxc6"  "de" "Ng4"
                     "O-O"))))
        (test-not (chess-check? game))
        (test-not (chess-mate? game)))
      '(test-move-black "" 2
         "........
         ........
         ........
         ........
         ........
         ........
         ........
         ........")
      (test-move-white "e8" 1
        "........
         ....P...
         .....K..
         ...k....
         ........
         ........
         ........
         ........")
      (test-skip
       "a little slow..."
       (test-move-white "Qxf7" 3
         "r.bqkb.r
         pppp.ppp
         ..n..n..
         ....p..Q
         ..B.P...
         ........
         PPPP.PPP
         RNB.K.NR"))
      (test-move-black "Nxh5" 2
        "r.bqkb.r
         pppp.ppp
         ..n..n..
         ....p..Q
         ..B.P...
         ........
         PPPP.PPP
         RNB.K.NR")
      (test-end))))

;; Local Variables:
;; eval: (put 'test-move-white 'scheme-indent-function 2)
;; eval: (put 'test-move-black 'scheme-indent-function 2)
;; End:
