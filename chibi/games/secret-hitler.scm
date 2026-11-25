
(define-record-type Game
  (%make-game players cards num-liberal-policies num-fascist-policies)
  game?
  (players game-players)
  (cards game-cards game-cards-set!)
  (num-liberal-policies game-num-liberal-policies
                        game-num-liberal-policies-set!)
  (num-fascist-policies game-num-fascist-policies
                        game-num-fascist-policies-set!))

(define (game-num-players game)
  (vector-length (game-players game)))

(define (game-num-living-players game)
  (let* ((vec (game-players game))
         (len (vector-length vec)))
    (let lp ((i 0) (res 0))
      (if (>= i len)
          res
          (lp (+ i 1) (if (player-alive? (vector-ref vec i)) (+ res 1) res))))))

(define (game-player game i)
  (vector-ref (game-players game) i))

(define (game-add-policy! game policy)
  (if (eq? policy 'liberal)
      (game-num-liberal-policies-set!
       game (+ 1 (game-num-liberal-policies game)))
      (game-num-fascist-policies-set!
       game (+ 1 (game-num-fascist-policies game)))))

(define-record-type Player
  (%make-player id role alive? strategy beliefs)
  player?
  (id player-id)
  (role player-role)
  (alive? player-alive? player-set-alive?!)
  (strategy player-strategy player-strategy-set!)
  (beliefs player-beliefs))

(define (player-liberal? player)
  (eq? 'liberal (player-role player)))
(define (player-fascist? player)
  (not (player-liberal? player)))
(define (player-hitler? player)
  (eq? 'hitler (player-role player)))

(define-record-type Strategy
  (make-strategy init update-beliefs vote legislate enact
                 choose-president choose-chancellor investigate peek execute)
  strategy?
  (init strategy-init)
  (update-beliefs strategy-update-beliefs)
  (vote strategy-vote)
  (legislate strategy-legislate)
  (enact strategy-enact)
  (choose-president strategy-choose-president)
  (choose-chancellor strategy-choose-chancellor)
  (investigate strategy-investigate)
  (peek strategy-peek)
  (execute strategy-execute))

(define (player-vote player)
  (strategy-vote (player-strategy player)))

(define (belief-liberal? beliefs id)
  (vector-ref beliefs (* 2 id)))
(define (belief-set-liberal?! beliefs id x)
  (vector-set! beliefs (* 2 id) x))
(define (belief-hitler? beliefs id)
  (vector-ref beliefs (+ 1 (* 2 id))))
(define (belief-set-hitler?! beliefs id x)
  (vector-set! beliefs (+ 1 (* 2 id)) x))

(define (player-trust player other-id)
  ((if (player-liberal? player) (lambda (x) x) (lambda (x) (- 1.0 x)))
   (belief-liberal? (player-beliefs player) other-id)))

(define (player-increase-belief-liberal? player other-id rate)
  (let ((beliefs (player-beliefs player)))
    (belief-set-liberal?! beliefs other-id (* rate (belief-liberal? beliefs other-id)))))
(define (player-decrease-belief-liberal? player other-id rate)
  (let ((beliefs (player-beliefs player)))
    (belief-set-liberal?! beliefs other-id (- 1.0 (* rate (- 1.0 (belief-liberal? beliefs other-id)))))))

(define total-num-liberal-cards 6)
(define total-num-fascist-cards 11)

(define (make-beliefs num-players self-id self-role)
  (let ((res (make-vector (* 2 num-players) 0.5)))
    (belief-set-liberal?! res self-id (if (eq? self-role 'liberal) 1.0 0.0))
    (belief-set-hitler?! res self-id (if (eq? self-role 'hitler) 1.0 0.0))
    res))

(define (make-player id role strategy num-players)
  (%make-player id role #t strategy (make-beliefs num-players id role)))

(define (make-game num-players . o)
  (let* ((liberal-strategy (if (pair? o) (car o) strategy/simple))
         (o (if (pair? o) (cdr o) '()))
         (fascist-strategy (if (pair? o) (car o) strategy/simple))
         (o (if (pair? o) (cdr o) '()))
         (hitler-strategy (if (pair? o) (car o) liberal-strategy))
         (num-fascists (case num-players ((5 6) 1) ((7 8) 2) (else 3)))
         (num-liberals (- num-players num-fascists 1))
         (players (shuffle-players num-fascists
                                   num-liberals
                                   liberal-strategy
                                   fascist-strategy
                                   hitler-strategy))
         (cards
          (shuffle-cards total-num-liberal-cards total-num-fascist-cards)))
    (%make-game players cards 0 0)))

(define (game-count-role game role)
  (do ((i 0 (+ i 1))
       (count 0 (+ count (if (eq? role (player-role (game-player game i))) 1 0))))
      ((>= i (game-num-players game))
       count)))

(define (game-hitler-id game)
  (let lp ((i (- (game-num-players game) 1)))
    (and (>= i 0)
         (let ((player (game-player game i)))
           (if (eq? 'hitler (player-role player))
               i
               (lp (- i 1)))))))

(define (game-player-highest-trust game player exceptions)
  (let lp ((i (- (game-num-players game) 1)) (highest-id #f) (trust -1.0) (run 1))
    (cond
     ((< i 0)
      highest-id)
     ((or (= i (player-id player)) (memq i exceptions))
      (lp (- i 1) highest-id trust run))
     (else
      (let ((trust-i (player-trust player i)))
        (cond
         ((> trust-i trust)
          (lp (- i 1) i trust-i 1))
         ((= trust-i trust)
          (if (zero? (random-integer (+ run 1)))
              (lp (- i 1) i trust-i (+ run 1))
              (lp (- i 1) highest-id trust (+ run 1))))
         (else
          (lp (- i 1) highest-id trust 1))))))))

(define (game-player-lowest-trust game player)
  (let lp ((i (- (game-num-players game) 1)) (lowest-id #f) (trust 10.0) (run 1))
    (cond
     ((< i 0)
      lowest-id)
     ((= i (player-id player))
      (lp (- i 1) lowest-id trust run))
     (else
      (let ((trust-i (player-trust player i)))
        (cond
         ((< trust-i trust)
          (lp (- i 1) i trust-i 1))
         ((= trust-i trust)
          (if (zero? (random-integer (+ run 1)))
              (lp (- i 1) i trust-i (+ run 1))
              (lp (- i 1) lowest-id trust (+ run 1))))
         (else
          (lp (- i 1) lowest-id trust 1))))))))

(define (game-player-least-known-trust game player)
  (let lp ((i (- (game-num-players game) 1)) (least-known-id #f) (known 1.0) (run 1))
    (cond
     ((< i 0)
      least-known-id)
     ((= i (player-id player))
      (lp (- i 1) least-known-id known run))
     (else
      (let ((known-i (abs (- 0.5 (player-trust player i)))))
        (cond
         ((< known-i known)
          (lp (- i 1) i known-i 1))
         ((= known-i known)
          (if (zero? (random-integer (+ run 1)))
              (lp (- i 1) i known-i (+ run 1))
              (lp (- i 1) least-known-id known (+ run 1))))
         (else
          (lp (- i 1) least-known-id known 1))))))))

(define (shuffle-players num-fascists
                         num-liberals
                         liberal-strategy
                         fascist-strategy
                         hitler-strategy)
  (let ((num-players (+ 1 num-fascists num-liberals)))
    (let lp ((i 0)
             (hitler-assigned? #f)
             (num-fascists num-fascists)
             (num-liberals num-liberals)
             (res '()))
      (cond
       ((>= i num-players)
        (let ((res (list->vector (reverse res))))
          (do ((i 0 (+ i 1)))
              ((= i num-players) res)
            (let ((player (vector-ref res i)))
              (if (or (eq? 'fascist (player-role player))
                      (and (eq? 'hitler (player-role player))
                           (<= num-players 6)))
                  (let ((beliefs (player-beliefs player)))
                    (do ((j 0 (+ j 1)))
                        ((= j num-players))
                      (let ((other (vector-ref res j)))
                        (belief-set-liberal?!
                         beliefs j (if (player-liberal? other) 1.0 0.0))
                        (belief-set-hitler?!
                         beliefs j (if (player-hitler? other) 1.0 0.0))))))))))
       ((and (not hitler-assigned?)
             (zero? (random-integer (+ 1 num-fascists num-liberals))))
        (let ((hitler (make-player i 'hitler hitler-strategy num-players)))
          (lp (+ i 1) #t num-fascists num-liberals (cons hitler res))))
       ((and (positive? num-fascists)
             (< (random-integer (+ (if hitler-assigned? 1 0)
                                   num-fascists num-liberals))
                num-fascists))
        (let ((fascist (make-player i 'fascist fascist-strategy num-players)))
          (lp (+ i 1) hitler-assigned? (- num-fascists 1) num-liberals
              (cons fascist res))))
       (else
        (let ((liberal (make-player i 'liberal liberal-strategy num-players)))
          (lp (+ i 1) hitler-assigned? num-fascists (- num-liberals 1)
              (cons liberal res))))))))

(define (shuffle-cards num-liberals num-fascists)
  (let lp ((num-liberals num-liberals) (num-fascists num-fascists) (res '()))
    (cond
     ((and (zero? num-liberals) (zero? num-fascists))
      (reverse res))
     ((< (random-integer (+ num-liberals num-fascists)) num-liberals)
      (lp (- num-liberals 1) num-liberals (cons 'liberal res)))
     (else
      (lp num-liberals (- num-fascists 1) (cons 'fascist res))))))

(define (draw game n)
  (let lp ((i 0) (num-liberals 0))
    (cond
     ((>= i n)
      num-liberals)
     ((null? (game-cards game))
      (let ((cards (shuffle-cards (- total-num-liberal-cards
                                     (game-num-liberal-policies game)
                                     num-liberals)
                                  (- total-num-fascist-cards
                                     (game-num-fascist-policies game)
                                     (- i num-liberals)))))
        (game-cards-set! game cards)
        (lp i num-liberals)))
     (else
      (let ((num-liberals (if (eq? 'liberal (car (game-cards game)))
                              (+ 1 num-liberals)
                              num-liberals)))
        (game-cards-set! game (cdr (game-cards game)))
        (lp (+ i 1) num-liberals))))))

(define (vote-passed? game votes)
  (let ((quorum (game-num-living-players game)))
    (let lp ((i 0) (sum 0))
      (cond ((>= sum quorum))
            ((>= i quorum) #f)
            (else (lp (+ i 1) (if (vector-ref votes i) (+ sum 2) sum)))))))

(define (execute-player! game id)
  (when (integer? id)
    (player-set-alive?! (game-player game id) #f)))

(define (play-game! game . o)
  (let ((trace? (and (pair? o) (car o))))
    (vector-for-each
     (lambda (player)
       ((strategy-init (player-strategy player)) game player))
     (game-players game))
    (let lp ((president-id 0) (last-pres-id #f) (last-chan-id #f) (failed 0))
      (let* ((president (game-player game president-id))
             (chancellor-id ((strategy-choose-chancellor
                              (player-strategy president))
                             game
                             president
                             last-pres-id
                             last-chan-id))
             (chancellor (game-player game chancellor-id))
             (votes (vector-map
                     (lambda (player)
                       (if (player-alive? player)
                           ((player-vote player)
                            game player president-id chancellor-id)
                           #f))
                     (game-players game))))
        (define (next-president)
          (modulo (+ 1 president-id) (game-num-players game)))
        (define (executive-action passed?)
          (case (game-num-fascist-policies game)
            ((1)
             ;; investigate
             ((strategy-investigate (player-strategy president)) game president)
             (lp (next-president) president-id chancellor-id 0))
            ((2)
             ;; special election
             (lp ((strategy-choose-president (player-strategy president))
                  game president chancellor-id)
                 president-id
                 chancellor-id
                 0))
            ((3)
             ;; peek
             ((strategy-peek (player-strategy president)) game president)
             (lp (next-president) president-id chancellor-id 0))
            ((4 5)
             ;; execute
             (let ((id ((strategy-execute (player-strategy president))
                        game president chancellor-id)))
               (cond
                ((and (integer? id) (player-hitler? (game-player game id)))
                 'liberal-by-execution)
                (else
                 (execute-player! game id)
                 (lp (next-president) president-id chancellor-id 0)))))
            (else
             'fascist-by-vote)))
        (define (update-result result passed?)
          (game-add-policy! game result)
          (vector-for-each
           (lambda (player)
             ((strategy-update-beliefs (player-strategy player))
              game player president-id chancellor-id votes result))
           (game-players game))
          (cond
           ((>= (game-num-liberal-policies game) 5)
            'liberal-by-vote)
           ((eq? 'fascist result)
            (executive-action passed?))
           (result
            (lp (next-president) president-id chancellor-id 0))
           (else
            (lp (next-president) last-pres-id last-chan-id (+ 1 failed)))))
        (when trace?
          (write `(pres: ,president-id ,(player-role president)
                         chan: ,chancellor-id ,(player-role chancellor)
                         votes: ,votes
                         lib: ,(game-num-liberal-policies game)
                         fas: ,(game-num-fascist-policies game)))
          (newline))
        (cond
         ((vote-passed? game votes)
          (if (and (player-hitler? chancellor)
                   (>= (game-num-fascist-policies game) 3))
              'fascist-by-election
              (let* ((num-liberal-policies (draw game 3))
                     (num-liberals-legislated
                      ((strategy-legislate (player-strategy president))
                       game president president-id chancellor-id
                       num-liberal-policies))
                     (result
                      ((strategy-enact (player-strategy chancellor))
                       game chancellor president-id chancellor-id
                       num-liberals-legislated)))
                (when trace?
                  (write `(draw: ,num-liberal-policies result: ,result))
                  (newline))
                (update-result result #t))))
         ((>= 2 failed)
          (update-result (if (positive? (draw game 1)) 'liberal 'fascist) #f))
         (else
          (update-result #f #f)))))))

(define (play-series rounds num-players . o)
  (let* ((liberal-strategy (if (pair? o) (car o) strategy/simple))
         (o (if (pair? o) (cdr o) '()))
         (fascist-strategy (if (pair? o) (car o) strategy/simple))
         (o (if (pair? o) (cdr o) '()))
         (hitler-strategy (if (pair? o) (car o) liberal-strategy)))
    (let lp ((i 0) (totals '()))
      (if (>= i rounds)
          totals
          (let* ((game (make-game num-players liberal-strategy
                                  fascist-strategy hitler-strategy))
                 (res (play-game! game))
                 (cell (assq res totals)))
            (cond
             ((pair? cell)
              (set-cdr! cell (+ 1 (cdr cell)))
              (lp (+ i 1) totals))
             (else (lp (+ i 1) (cons (cons res 1) totals)))))))))

(define (play-human num-players . o)
  (let* ((liberal-strategy (if (pair? o) (car o) strategy/simple))
         (o (if (pair? o) (cdr o) '()))
         (fascist-strategy (if (pair? o) (car o) strategy/simple))
         (o (if (pair? o) (cdr o) '()))
         (hitler-strategy (if (pair? o) (car o) liberal-strategy)))
    (let* ((game (make-game num-players liberal-strategy
                            fascist-strategy hitler-strategy))
           (human-id (random-integer num-players))
           (human (game-player game human-id)))
      (player-strategy-set! human strategy/human)
      (play-game! game))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies

(define (update-beliefs/simple rate)
  (lambda (game player president-id chancellor-id votes policy)
    (when (and policy (player-liberal? player))
      (cond
       ((eq? 'fasict policy)
        (player-decrease-belief-liberal? player president-id rate)
        (player-decrease-belief-liberal? player chancellor-id rate))
       (else
        (player-increase-belief-liberal? player president-id rate)
        (player-increase-belief-liberal? player chancellor-id rate))))))

(define (vote/simple game player president-id chancellor-id)
  (>= (+ (player-trust player president-id)
         (player-trust player chancellor-id))
      1.0))

(define (legislate/simple game player president-id chancellor-id
                          num-liberal-policies)
  (case num-liberal-policies
    ((0) 0)
    ((3) 2)
    (else
     (if (player-liberal? player)
         num-liberal-policies
         (- num-liberal-policies 1)))))

(define (enact/simple game player president-id chancellor-id
                      num-liberal-policies)
  (case num-liberal-policies
    ((0) 'fascist)
    ((2) 'liberal)
    (else
     (if (player-liberal? player)
         'liberal
         'fascist))))

(define (choose-president/simple game player chancellor-id)
  (game-player-highest-trust game player '()))

(define (choose-chancellor/simple game player last-pres-id last-chan-id)
  (let ((hitler-id (game-hitler-id game))
        (pres-allowed? (<= (game-num-living-players game) 5)))
    (if (and (player-fascist? player)
             (not (player-hitler? player))
             (not (eq? hitler-id last-chan-id))
             (or pres-allowed? (not (eq? hitler-id last-pres-id))))
        hitler-id
        (game-player-highest-trust
         game
         player
         (if pres-allowed?
             (list last-chan-id)
             (list last-chan-id last-pres-id))))))

(define (investigate/simple game player)
  (let* ((other-id (game-player-least-known-trust game player))
         (belief-liberal
          (if (player-liberal? (game-player game other-id)) 1.0 0.0)))
    (belief-set-liberal?! (player-beliefs player) other-id belief-liberal)))

(define (peek/simple game player)
  #f)

(define (execute/simple game player chancellor-id)
  (game-player-lowest-trust game player))

(define strategy/simple
  (make-strategy
   values (update-beliefs/simple 0.5)
   vote/simple legislate/simple enact/simple
   choose-president/simple choose-chancellor/simple
   investigate/simple peek/simple execute/simple))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; human player

(define (print-line . o)
  (for-each display o)
  (newline))

(define (input-line prompt . o)
  (let ((validate (and (pair? o) (car o))))
    (let lp ()
      (display prompt)
      (flush-output-port)
      (let ((res (read-line)))
        (if (and validate (not (validate res)))
            (lp)
            res)))))

(define (input-boolean prompt)
  (member (input-line (string-append prompt " y/n: ")
                      (lambda (line) (member line '("y" "Y" "n" "N"))))
          '("y" "Y")))

(define (input-policy prompt)
  (if (member (input-line (string-append prompt " (l)iberal/(f)ascist: ")
                          (lambda (line) (member line '("l" "L" "f" "F"))))
              '("l" "L"))
      'liberal
      'fascist))

(define (input-player prompt game exceptions)
  (string->number
   (input-line prompt
               (lambda (line)
                 (let ((n (string->number line)))
                   (and n (not (memv n exceptions))))))))

(define (init/human game player)
  (print-line "You, player " (player-id player) " are "
              (cond ((player-hitler? player) "Hitler.")
                    ((player-fascist? player) "a fascist.")
                    (else "a liberal.")))
  (when (and (player-fascist? player)
             (or (not (player-hitler? player))
                 (<= (vector-length (game-players game)) 6)))
    (vector-for-each
     (lambda (p)
       (if (not (eq? (player-id p) (player-id player)))
           (print-line "Player " (player-id p) " is "
                       (cond ((player-hitler? p) "Hitler.")
                             ((player-fascist? p) "a fascist.")
                             (else "a liberal.")))))
     (game-players game))))

(define (update-beliefs/human game player president-id chancellor-id votes policy)
  (vector-for-each
   (lambda (p)
     (if (and (not (eq? (player-id p) (player-id player)))
              (player-alive? p))
         (print-line "Player " (player-id p) " voted "
                     (if (vector-ref votes (player-id p)) "yes." "no."))))
   (game-players game))
  (cond
   ((vote-passed? game votes)
    (print-line "The vote passed.")
    (print-line "President Player " president-id " and Chancellor Player "
                chancellor-id " enact a " policy " policy."))
   (else
    (print-line "The vote failed.")
    (when policy
      (print-line "A " policy " policy was enacted.")))))

(define (vote/human game player president-id chancellor-id)
  (print-line "President Player " president-id " and Chancellor Player "
              chancellor-id " are up for election.")
  (input-boolean "Do you vote for them?"))

(define (legislate/human game player president-id chancellor-id
                         num-liberal-policies)
  (case num-liberal-policies
    ((0) (print-line "You draw 3 fascist policies.") 0)
    ((3) (print-line "You draw 3 liberal policies.") 2)
    (else
     (print-line "You draw " num-liberal-policies " liberal policies, "
                 "and " (- 3 num-liberal-policies) " fascist policies.")
     (if (eq? 'liberal (input-policy "What policy do you discard?"))
         (- num-liberal-policies 1)
         num-liberal-policies))))

(define (enact/human game player president-id chancellor-id
                      num-liberal-policies)
  (case num-liberal-policies
    ((0) (print-line "You are given 2 fascist policies.") 'fascist)
    ((2) (print-line "You are given 2 liberal policies.") 'liberal)
    (else
     (print-line "You are given 1 liberal policy and 1 fascist policy.")
     (input-policy "What policy do you enact?"))))

(define (choose-president/human game player chancellor-id)
  (input-player "Who do you choose for the special election? " game
                (list (player-id player))))

(define (choose-chancellor/human game player last-pres-id last-chan-id)
  (input-player "Who do you choose for your chancellor? " game
                (list (player-id player)
                      last-chan-id
                      (and (<= (game-num-living-players game) 5)
                           last-pres-id))))

(define (investigate/human game player)
  (let* ((other-id (input-player "Who do you investigate? " game
                                 (list (player-id player))))
         (other (game-player game other-id)))
    (print-line "Player " other-id " is "
                (if (player-fascist? player) "a fascist." "a liberal."))))

(define (peek/human game player)
  #f)

(define (execute/human game player chancellor-id)
  (input-player "Who do you execute? " game '()))

(define strategy/human
  (make-strategy
   init/human update-beliefs/human
   vote/human legislate/human enact/human
   choose-president/human choose-chancellor/human
   investigate/human peek/human execute/human))
