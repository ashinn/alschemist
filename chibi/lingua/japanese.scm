
(define kana-by-group
  '((vowels
     (#\a #\あ #\ア #\ｱ #\ぁ #\ァ)
     (#\e #\え #\エ #\ｴ #\ぇ #\ェ)
     (#\i #\い #\イ #\ｲ #\ぃ #\ィ)
     (#\o #\お #\オ #\ｵ #\ぉ #\ォ)
     (#\u #\う #\ウ #\ｳ #\ぅ #\ゥ))
    (small-vowels
     (#\a #\ぁ #\ァ #\ｱ)
     (#\e #\ぇ #\ェ #\ｴ)
     (#\i #\ぃ #\ィ #\ｲ)
     (#\o #\ｵ #\ぉ #\ォ)
     (#\u #\ぅ #\ゥ #\ｳ))
    (#\k
     (#\a #\か #\カ #\ｶ #\ヵ)
     (#\e #\け #\ケ #\ｹ #\ヶ)
     (#\i #\き #\キ #\ｷ)
     (#\o #\こ #\コ #\ｺ)
     (#\u #\く #\ク #\ｸ))
    (#\g
     (#\a #\が #\ガ "ｶﾞ")
     (#\e #\げ #\ゲ "ｹﾞ")
     (#\i #\ぎ #\ギ "ｷﾞ")
     (#\o #\ご #\ゴ "ｺﾞ")
     (#\u #\ぐ #\グ "ｸﾞ"))
    (#\s
     (#\a #\さ #\サ #\ｻ)
     (#\e #\せ #\セ #\ｾ)
     (#\i #\し #\シ #\ｼ)
     (#\o #\そ #\ソ #\ｿ)
     (#\u #\す #\ス #\ｽ))
    (#\z
     (#\a #\ざ #\ザ "ｻﾞ")
     (#\e #\ぜ #\ゼ "ｾﾞ")
     (#\i #\じ #\ジ "ｼﾞ")
     (#\o #\ぞ #\ゾ "ｿﾞ")
     (#\u #\ず #\ズ "ｽﾞ"))
    (#\t
     (#\a #\た #\タ #\ﾀ)
     (#\e #\て #\テ #\ﾃ)
     (#\i #\ち #\チ #\ﾁ)
     (#\o #\と #\ト #\ﾄ)
     (#\u #\つ #\ツ #\ﾂ #\っ #\ッ))
    (chisai-tsu
     (#\u #\っ #\ッ #\ﾂ))
    (#\d
     (#\a #\だ #\ダ "ﾀﾞ")
     (#\e #\で #\デ "ﾃﾞ")
     (#\i #\ぢ #\ヂ "ﾁﾞ")
     (#\o #\ど #\ド "ﾄﾞ")
     (#\u #\づ #\ヅ "ﾂﾞ"))
    (#\h
     (#\a #\は #\ハ #\ﾊ)
     (#\e #\へ #\ヘ #\ﾍ)
     (#\i #\ひ #\ヒ #\ﾋ)
     (#\o #\ほ #\ホ #\ﾎ)
     (#\u #\ふ #\フ #\ﾌ))
    (#\b
     (#\a #\ば #\バ "ﾊﾞ")
     (#\e #\べ #\ベ "ﾍﾞ")
     (#\i #\び #\ビ "ﾋﾞ")
     (#\o #\ぼ #\ボ "ﾎﾞ")
     (#\u #\ぶ #\ブ "ﾌﾞ"))
    (#\p
     (#\a #\ぱ #\パ "ﾊﾟ")
     (#\e #\ぺ #\ペ "ﾍﾟ")
     (#\i #\ぴ #\ピ "ﾋﾟ")
     (#\o #\ぽ #\ポ "ﾎﾟ")
     (#\u #\ぷ #\プ "ﾌﾟ"))
    (#\m
     (#\a #\ま #\マ #\ﾏ)
     (#\e #\め #\メ #\ﾒ)
     (#\i #\み #\ミ #\ﾐ)
     (#\o #\も #\モ #\ﾓ)
     (#\u #\む #\ム #\ﾑ))
    (#\n
     (#\a #\な #\ナ #\ﾅ)
     (#\e #\ね #\ネ #\ﾈ)
     (#\i #\に #\ニ #\ﾆ)
     (#\o #\の #\ノ #\ﾉ)
     (#\u #\ぬ #\ヌ #\ﾇ))
    (#\r
     (#\a #\ら #\ラ #\ﾗ)
     (#\e #\れ #\レ #\ﾚ)
     (#\i #\り #\リ #\ﾘ)
     (#\o #\ろ #\ロ #\ﾛ)
     (#\u #\る #\ル #\ﾙ))
    (#\w
     (#\a #\わ #\ワ #\ﾜ)
     (#\e #\ゑ #\ヱ #\ｴ)
     (#\i #\ゐ #\ヰ #\ｲ)
     (#\o #\を #\ヲ #\ｦ)
     (#\u #\う #\ウ #\ｳ))
    (#\y
     (#\a #\や #\ヤ #\ﾔ #\ゃ #\ャ)
     (#\o #\よ #\ヨ #\ﾖ #\ょ #\ョ)
     (#\u #\ゆ #\ユ #\ﾕ #\ゅ #\ュ))
    (chisai-y
     (#\a #\ゃ #\ャ #f)
     (#\o #\ょ #\ョ #f)
     (#\u #\ゅ #\ュ #f))
    (n
     (#\n #\ん #\ン #\ﾝ))
    ))

(define (kana-pred get)
  (lambda (c)
    (any (lambda (g) (any (lambda (l) (eq? c (get l))) (cdr g))) kana-by-group)))

(define hiragana-char? (kana-pred cadr))
(define katakana-char? (kana-pred caddr))
(define hankaku-char? (kana-pred cadddr))

;; utility to generate closures acting on kana-by-group
(define (kana-map from to)
  (lambda (c)
    (let loop ((g kana-by-group))
      (cond ((null? g)
             #f)
            ((find (lambda (l) (eq? (from l) c)) (cdar g))
             => to)
            (else
             (loop (cdr g)))))))

(define (hiragana-char->katakana c)
  (or ((kana-map cadr caddr) c) c))
(define (katakana-char->hiragana c)
  (or ((kana-map caddr cadr) c) c))
(define (katakana-char->hankaku c)
  (or ((kana-map caddr cadddr) c) c))
(define (hankaku-char->katakana c)
  (or ((kana-map cadddr caddr) c) c))

(define (hiragana-char->small-hiragana c)
  (or ((kana-map second (cut list-ref <> 4)) c) c))
(define (katakana-char->small-katakana c)
  (or ((kana-map third (cut list-ref <> 5)) c) c))

(define hiragana->katakana (cut string-map hiragana-char->katakana <>))
(define katakana->hiragana (cut string-map katakana-char->hiragana <>))
(define hankaku->katakana  (cut string-map hankaku-char->katakana <>))

;; hankaku may need multiple chars per full-width char
(define (katakana->hankaku str)
  (string-concatenate
   (map (lambda (c) (if (char? c) (string c) c))
        (map katakana-char->hankaku (string->list str)))))

(define (kana-romaji-converter k)
  (cond ((hiragana-char? k) romaji->hiragana)
        ((katakana-char? k) romaji->katakana)
        ((hankaku-char? k)  romaji->hankaku)
        (else #f)))

(define (kana-dakuten-char k)
  (and-let* ((conv (kana-romaji-converter k))
             (str (string->list (kana->romaji (string k))))
             (c1 (car str)))
    (conv (list->string
           (case c1
             ((#\k) (cons #\g (cdr str)))
             ((#\h #\p) (cons #\b (cdr str)))
             ((#\t)
              (if (eq? (cadr str) #\h)
                (cons #\c (cons #\h (cddr str)))
                (cons #\d (cdr str))))
             ((#\s)
              (if (eq? (cadr str) #\h)
                (cons #\j (cddr str))
                (cons #\z (cdr str))))
             (else str))))))

(define (kana-handakuten-char k)
  (and-let* ((conv (kana-romaji-converter k))
             (str (string->list (kana->romaji (string k))))
             (c1 (car str)))
    (conv (list->string
           (case c1
             ((#\h #\b) (cons #\p (cdr str)))
             (else str))))))

(define (kana-no-dakuten-char k)
  (and-let* ((conv (kana-romaji-converter k))
             (str (string->list (kana->romaji (string k))))
             (c1 (car str)))
    (conv (list->string
           (case c1
             ((#\g) (cons #\k (cdr str)))
             ((#\b #\p) (cons #\h (cdr str)))
             ((#\c)
              (cons #\t (cddr str)))
             ((#\d)
              (cons #\t (cdr str)))
             ((#\j)
              (cons #\s (cdr str)))
             ((#\z)
              (cons #\s (cdr str)))
             (else str))))))

(define kana-base-initial-chars
  '(#\k #\s #\t #\h #\m #\n #\r #\w #\y))
(define kana-all-initial-chars
  '(#\k #\g #\s #\z #\t #\d #\h #\b #\p #\m #\n #\r #\w #\y))

(define (join-vowel-group g c)
  (or (and-let* ((c-group (assq c g)))
        (filter char? (cdr c-group)))
      '()))

;; convert a regexp string, translating western vowel sounds into any
;; japanese (syllabic) kana ending in that vowel sound.
(define vowel->kana-match
  (let ((a (apply append (map (cut join-vowel-group <> #\a) kana-by-group)))
        (i (apply append (map (cut join-vowel-group <> #\i) kana-by-group)))
        (u (apply append (map (cut join-vowel-group <> #\u) kana-by-group)))
        (e (apply append (map (cut join-vowel-group <> #\e) kana-by-group)))
        (o (apply append (map (cut join-vowel-group <> #\o) kana-by-group))))
    (lambda (s)
      (letrec ((kons
                (lambda (c tail)
                  (case c
                    ((#\a #\A #\ａ #\Ａ)
                     (append '(#\]) a '(#\[) tail))
                    ((#\i #\I #\ｉ #\Ｉ)
                     (append '(#\]) i '(#\[) tail))
                    ((#\u #\U #\ｕ #\Ｕ)
                     (append '(#\]) u '(#\[) tail))
                    ((#\e #\E #\ｅ #\Ｅ)
                     (append '(#\]) e '(#\[) tail))
                    ((#\o #\O #\ｏ #\Ｏ)
                     (append '(#\]) o '(#\[) tail))
                    (else
                     (cons c tail))))))
        (reverse-list->string (string-fold kons '() s))))))

;; convert a regexp string, translating western consonant sounds into
;; any japanese (syllabic) kana beginning with that consonant sound.
(define (consonant->kana-match s)
  (letrec ((kons
            (lambda (c tail)
              (or (and-let* ((g (assq c kana-by-group)))
                    (append '(#\]) (apply append (map cdr (cdr g)))
                            '(#\[) tail))
                  (cons c tail)))))
    (reverse-list->string (string-fold kons '() (string-downcase s)))))

(define (vowel-replace s from to)
  (let ((r (kana->kunrei-romaji s)))
    (romaji->hiragana
     (string-map (lambda (c) (if (eq? c from) to c)) r))))

;; return the canonical (one or two) char spelling of a kana
(define (kana-char->romaji c)
  (let loop ((g kana-by-group))
    (cond ((null? g)
           #f)
          ((find (lambda (l) (memq c l)) (cdar g))
           => (lambda (x)
                (cond ((char? (caar g))
                       (cons (caar g) (list (car x))))
                      (else (list (car x))))))
          (else
           (loop (cdr g))))))

(define (kana->kunrei-list word)
  (let loop ((chars (string->list word)))
    (if (null? chars)
      '()
      (let* ((c (car chars))
             (c2 (kana-char->romaji c))
             (tail (loop (cdr chars))))
        (cond ((memq c '(#\っ #\ッ))
               ;; handle consonant doubling w/ chisai tsu
               (if (pair? (cdr tail))
                 (cons (car tail) tail)
                 '()))
              ((and (pair? (cdr chars))
                    (memq (cadr chars) '(#\ゃ #\ょ #\ゅ #\ャ #\ョ #\ュ)))
               ;; blend litte y's
               (cons (car c2) (cons #\y (cdr tail))))
              (else
               ;; just append the sounds
               (append (or c2 (if (char-alphabetic? c) (list c) '()))
                       tail)))))))

(define (kana->kunrei-romaji word)
  (list->string (kana->kunrei-list word)))

;; convert common alternate spellings for i kana
(define (kana-i-char c)
  (case c
    ((#\s) '(#\s #\h))
    ((#\z) '(#\j))
    ((#\t) '(#\c #\h))
    (else (list c))))

(define (kana-redundant a b)
  (cond ((and (eqv? a #\t) (eqv? b #\s)) #\t)
        ((and (eqv? a #\d) (eqv? b #\z)) #\d)
        (else #f)))

;; convert a word in any kana to hepburn romaji spelling
(define (kana->hepburn-romaji word)
  (list->string
   (let loop ((ls (kana->kunrei-list word)))
     (if (null? ls)
       '()
       (let ((c (car ls))
             (rest (cdr ls)))
         (cond ((and (eq? c #\t) (pair? rest) (eq? #\u (car rest)))
                (append '(#\t #\s #\u) (loop (cdr rest))))
               ((and (eq? c #\h) (pair? rest) (eq? #\u (car rest)))
                (append '(#\f #\u) (loop (cdr rest))))
               ((and (memq c '(#\s #\z #\t #\d)) (pair? rest))
                (case (car rest)
                  ((#\i)
                   (append (kana-i-char c) (loop rest)))
                  ((#\y)
                   (append (kana-i-char c) (loop (cdr rest))))
                  (else (cons c (loop rest)))))
               (else
                (cons c (loop rest)))))))))

;; use hepburn by default
(define kana->romaji kana->hepburn-romaji)

(define (normalize-romaji-char c)
  (case c ((#\l) #\r) ((#\f) #\h) (else c)))

;; just lookup from kana-by-group
(define (romaji-complete-basic ch tail)
  (if (null? tail)
    (values #f (list ch))
    (let* ((g (find (lambda (x) (eqv? (car x) ch)) kana-by-group))
           (ch2 (car tail)))
      (if g
        (cond ((eqv? ch2 #\y)
               (let ((l (find (lambda (x) (eqv? #\i (car x))) (cdr g))))
                 (if (and l (pair? (cdr tail)))
                   (values (cadr l) (cons #\x tail))
                   (values #f (cons ch tail)))))
              ((eqv? ch ch2)
               (values #\っ tail))
              (else
               (let ((l (find (lambda (x) (eqv? ch2 (car x))) (cdr g))))
                 (if l
                   (values (cadr l) (cdr tail))
                   (values #f tail)))))
        (values #f tail)))))

;; construct hiragana from kunrei, hepburn or wapuro
(define (romaji-complete1 ls)
  (if (null? ls)
    (values #f '())
    (let ((ch (normalize-romaji-char (car ls)))
          (tail (cdr ls)))
      (and-let* (((pair? tail))
                 (norm (kana-redundant ch (car tail))))
        (set! ch norm)
        (set! tail (cdr tail)))
      (case ch
        ((#\a)
         (values #\あ (cdr ls)))
        ((#\e)
         (values #\え (cdr ls)))
        ((#\i)
         (values #\い (cdr ls)))
        ((#\o)
         (values #\お (cdr ls)))
        ((#\u)
         (values #\う (cdr ls)))
        ((#\c #\s)
         (if (null? tail)
           (values #f ls)
           (let ((ch2 (car tail)))
             (if (eq? ch2 #\h)
               (let ((tail2 (cdr tail)))
                 (if (null? tail2)
                   (values #f ls)
                   (case (car tail2)
                     ((#\i) (values (if (eq? ch #\c) #\ち #\し) (cdr tail2)))
                     ((#\y) (values (if (eq? ch #\c) #\ち #\し) (cons #\x tail2)))
                     ((#\e) (values (if (eq? ch #\c) #\ち #\し) (cons #\x tail2)))
                     ((#\a #\u #\o) (values (if (eq? ch #\c) #\ち #\し)
                                            (cons #\x (cons #\y tail2)))))))
               (if (eq? ch #\c)
                 (if (eq? ch2 #\c)
                   (values #\っ tail)
                   (romaji-complete1 tail))
                 (romaji-complete-basic ch tail))))))
        ((#\j)
         (if (null? tail)
           (values #f ls)
           (case (car tail)
             ((#\i) (values #\じ (cdr tail)))
             ((#\y) (values #\じ (cons #\x tail)))
             ((#\e) (values #\じ (cons #\x tail)))
             ((#\a #\u #\o) (values #\じ (cons #\x (cons #\y tail))))
             (else  (values #f tail)))))
        ((#\n #\m)
         (if (null? tail)
           (values #f ls)
           (let ((ch2 (car tail)))
             (case ch2
               ((#\n #\m)
                (let ((tail2 (cdr tail)))
                  (if (and (null? tail2) (eqv? ch ch2))
                    (values #\ん '())
                    (values #\ん tail))))
               ((#\y)
                (let ((tail2 (cdr tail)))
                  (if (null? tail2)
                    (values #f ls)
                    (if (memv (car tail2) '(#\a #\e #\i #\o #\u))
                      (values (if (eq? ch #\n) #\に #\み) (cons #\x tail))
                      (values #\ん tail2)))))
               ((#\a)
                (values (if (eq? ch #\n) #\な #\ま) (cdr tail)))
               ((#\e)
                (values (if (eq? ch #\n) #\ね #\め) (cdr tail)))
               ((#\i)
                (values (if (eq? ch #\n) #\に #\み) (cdr tail)))
               ((#\o)
                (values (if (eq? ch #\n) #\の #\も) (cdr tail)))
               ((#\u)
                (values (if (eq? ch #\n) #\ぬ #\む) (cdr tail)))
               (else
                (values #\ん tail))))))
        ((#\x)
         (if (pair? (cdr ls))
           (receive (next tail2) (romaji-complete1 tail)
             (if next
               (values (hiragana-char->small-hiragana next) tail2)
               (values #f ls)))
           (values #f ls)))
        (else
         (romaji-complete-basic ch tail))))))

(define (romaji-complete word)
  (let loop ((res '())
             (ls (string->list word)))
    (receive (kana romaji) (romaji-complete1 ls)
      (if kana
        (loop (cons kana res) romaji)
        (values (apply string (reverse res)) (list->string romaji))))))

(define (romaji->hiragana word)
  (receive (kana romaji) (romaji-complete word)
    (if (memv (string-ref romaji 0) '(#\m #\n #\M #\N))
        (string-append kana "ん")
        kana)))

(define romaji->katakana
  (lambda (x) (hiragana->katakana (romaji->hiragana x))))
(define romaji->hankaku
  (lambda (x) (katakana->hankaku (romaji->katakana x))))
