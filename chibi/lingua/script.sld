
(define-library (chibi lingua script)
  (import (scheme base) (chibi iset base))
  (export
   char-script
   script->char-set
   char-set:adlam
   char-set:ahom
   char-set:anatolian-hieroglyphs
   char-set:arabic
   char-set:armenian
   char-set:avestan
   char-set:balinese
   char-set:bamum
   char-set:bassa-vah
   char-set:batak
   char-set:bengali
   char-set:bhaiksuki
   char-set:bopomofo
   char-set:brahmi
   char-set:braille
   char-set:buginese
   char-set:buhid
   char-set:canadian-aboriginal
   char-set:carian
   char-set:caucasian-albanian
   char-set:chakma
   char-set:cham
   char-set:cherokee
   char-set:chorasmian
   char-set:common
   char-set:coptic
   char-set:cuneiform
   char-set:cypriot
   char-set:cyrillic
   char-set:deseret
   char-set:devanagari
   char-set:dives-akuru
   char-set:dogra
   char-set:duployan
   char-set:egyptian-hieroglyphs
   char-set:elbasan
   char-set:elymaic
   char-set:ethiopic
   char-set:georgian
   char-set:glagolitic
   char-set:gothic
   char-set:grantha
   char-set:greek
   char-set:gujarati
   char-set:gunjala-gondi
   char-set:gurmukhi
   char-set:han
   char-set:hangul
   char-set:hanifi-rohingya
   char-set:hanunoo
   char-set:hatran
   char-set:hebrew
   char-set:hiragana
   char-set:imperial-aramaic
   char-set:inherited
   char-set:inscriptional-pahlavi
   char-set:inscriptional-parthian
   char-set:javanese
   char-set:kaithi
   char-set:kannada
   char-set:katakana
   char-set:kayah-li
   char-set:kharoshthi
   char-set:khitan-small-script
   char-set:khmer
   char-set:khojki
   char-set:khudawadi
   char-set:lao
   char-set:latin
   char-set:lepcha
   char-set:limbu
   char-set:linear-a
   char-set:linear-b
   char-set:lisu
   char-set:lycian
   char-set:lydian
   char-set:mahajani
   char-set:makasar
   char-set:malayalam
   char-set:mandaic
   char-set:manichaean
   char-set:marchen
   char-set:masaram-gondi
   char-set:medefaidrin
   char-set:meetei-mayek
   char-set:mende-kikakui
   char-set:meroitic-cursive
   char-set:meroitic-hieroglyphs
   char-set:miao
   char-set:modi
   char-set:mongolian
   char-set:mro
   char-set:multani
   char-set:myanmar
   char-set:nabataean
   char-set:nandinagari
   char-set:new-tai-lue
   char-set:newa
   char-set:nko
   char-set:nushu
   char-set:nyiakeng-puachue-hmong
   char-set:ogham
   char-set:ol-chiki
   char-set:old-hungarian
   char-set:old-italic
   char-set:old-north-arabian
   char-set:old-permic
   char-set:old-persian
   char-set:old-sogdian
   char-set:old-south-arabian
   char-set:old-turkic
   char-set:oriya
   char-set:osage
   char-set:osmanya
   char-set:pahawh-hmong
   char-set:palmyrene
   char-set:pau-cin-hau
   char-set:phags-pa
   char-set:phoenician
   char-set:psalter-pahlavi
   char-set:rejang
   char-set:runic
   char-set:samaritan
   char-set:saurashtra
   char-set:sharada
   char-set:shavian
   char-set:siddham
   char-set:signwriting
   char-set:sinhala
   char-set:sogdian
   char-set:sora-sompeng
   char-set:soyombo
   char-set:sundanese
   char-set:syloti-nagri
   char-set:syriac
   char-set:tagalog
   char-set:tagbanwa
   char-set:tai-le
   char-set:tai-tham
   char-set:tai-viet
   char-set:takri
   char-set:tamil
   char-set:tangut
   char-set:telugu
   char-set:thaana
   char-set:thai
   char-set:tibetan
   char-set:tifinagh
   char-set:tirhuta
   char-set:ugaritic
   char-set:vai
   char-set:wancho
   char-set:warang-citi
   char-set:yezidi
   char-set:yi
   char-set:zanabazar-square
   )
  (include "script.scm")
  (begin
    ;;> Returns the script name as a symbol for \var{ch}, with most
    ;;> punctuation being in the \scheme{'common} script.
    (define (char-script ch)
      (let ((n (char->integer ch))
            (end (- (vector-length range-ends) 1)))
        (cond
         ((<= n (vector-ref range-ends 0))
          (vector-ref range-scripts 0))
         ((> n (vector-ref range-ends end))
          'unknown)
         (else
          (let lp ((lo 1) (hi end))
            (if (>= lo hi)
                (vector-ref range-scripts lo)
                (let* ((mid (quotient (+ lo hi) 2))
                       (x (vector-ref range-ends mid)))
                  (cond
                   ((< n x) (lp lo mid))
                   ((> n x) (lp (+ mid 1) hi))
                   (else (vector-ref range-scripts mid))))))))))))
