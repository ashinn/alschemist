
;; カッコ内の西暦は、「大化」の始期（皇極天皇4年6月19日/645年7月17日）から「天正」の始期（元亀4年7月28日/1573年8月25日）まではユリウス暦で、「天正」の終期（天正20年12月8日/1593年1月10日）以降はグレゴリオ暦で表記している。これは、ヨーロッパのカトリック諸国で、天正年間に当たる1582年（天正10年）に、従来のユリウス暦からグレゴリオ暦が導入されたためである[注釈 2]。

;; 日本では1872年（ほぼ明治5年に当たる[注釈 4]）に採用され、明治5年12月2日（旧暦）の翌日を、明治6年1月1日（新暦）（グレゴリオ暦の1873年1月1日）とした。

(define (nengo-instant x) (vector-ref x 0))
(define (nengo-name x) (vector-ref x 1))
(define (nengo-yomi x) (vector-ref x 2))
(define (nengo-start-year x) (vector-ref x 3))
(define (nengo-start-month x) (vector-ref x 4))
(define (nengo-start-day x) (vector-ref x 5))

;; The instant is midnight (JST) of the listed reference western date,
;; which is Julian up through 天正 (208) and Gregorian thereafter.
;; The Japanese calendar did not switch to Gregorian leap year
;; counting until 1872, at which point it jumped from Meiji 5 Dec 12th
;; to Meiji 6 Jan 1st the next day.
(define japanese-nengo
  ;; start-instant name yomigana start-year start-month start-day
  '#(#(-inf.0 "大化前" "たいかまえ" 0 0 0)
     #(-41795686800 "大化" "たいか" 645 7 17)
     #(-41647942800 "白雉" "はくち" 650 3 22)
     #(-41500371600 "白雉と朱鳥の間" "はくちとしゅちょうのあいだ" 654 11 24)
     #(-40499341200 "朱鳥" "しゅちょう" 686 8 14)
     #(-40495194000 "朱鳥と大宝の間" "しゅちょうとたいほうのあいだ" 686 10 1)
     #(-40034941200 "大宝" "たいほう" 701 5 3)
     #(-39936358800 "慶雲" "けいうん" 704 6 16)
     #(-39821360400 "和銅" "わどう" 708 2 7)
     #(-39579872400 "霊亀" "れいき" 715 10 3)
     #(-39509715600 "養老" "ようろう" 717 12 24)
     #(-39314278800 "神亀" "じんき" 724 3 3)
     #(-39140787600 "天平" "てんぴょう" 729 9 2)
     #(-38520090000 "天平感宝" "てんぴょうかんぽう" 749 5 4)
     #(-38510845200 "天平勝宝" "てんぴょうしょうほう" 749 8 19)
     #(-38256829200 "天平宝字" "てんぴょうほうじ" 757 9 6)
     #(-38023117200 "天平神護" "てんぴょうじんご" 765 2 1)
     #(-37940605200 "神護景雲" "じんごけいうん" 767 9 13)
     #(-37842454800 "宝亀" "ほうき" 770 10 23)
     #(-37518368400 "天応" "てんおう" 781 1 30)
     #(-37465750800 "延暦" "えんりゃく" 782 9 30)
     #(-36718218000 "大同" "だいどう" 806 6 8)
     #(-36580410000 "弘仁" "こうにん" 810 10 20)
     #(-36160592400 "天長" "てんちょう" 824 2 8)
     #(-35844454800 "承和" "じょうわ" 834 2 14)
     #(-35389472400 "嘉祥" "かしょう" 848 7 16)
     #(-35298752400 "仁寿" "にんじゅ" 851 6 1)
     #(-35186346000 "斉衡" "さいこう" 854 12 23)
     #(-35115757200 "天安" "てんあん" 857 3 20)
     #(-35047328400 "貞観" "じょうがん" 859 5 20)
     #(-34478298000 "元慶" "がんぎょう" 877 6 1)
     #(-34232922000 "仁和" "にんな" 885 3 11)
     #(-34099779600 "寛平" "かんぴょう" 889 5 30)
     #(-33816560400 "昌泰" "しょうたい" 898 5 20)
     #(-33713053200 "延喜" "えんぎ" 901 8 31)
     #(-33026864400 "延長" "えんちょう" 923 5 29)
     #(-32775526800 "承平" "じょうへい" 931 5 16)
     #(-32551405200 "天慶" "てんぎょう" 938 6 22)
     #(-32270691600 "天暦" "てんりゃく" 947 5 15)
     #(-31938742800 "天徳" "てんとく" 957 11 21)
     #(-31835062800 "応和" "おうわ" 961 3 5)
     #(-31725853200 "康保" "こうほう" 964 8 19)
     #(-31597894800 "安和" "あんな" 968 9 8)
     #(-31545882000 "天禄" "てんろく" 970 5 3)
     #(-31428896400 "天延" "てんえん" 974 1 16)
     #(-31347853200 "貞元" "じょうげん" 976 8 11)
     #(-31272512400 "天元" "てんげん" 978 12 31)
     #(-31133408400 "永観" "えいかん" 983 5 29)
     #(-31071200400 "寛和" "かんな" 985 5 19)
     #(-31009251600 "永延" "えいえん" 987 5 5)
     #(-30935120400 "永祚" "えいそ" 989 9 10)
     #(-30896845200 "正暦" "しょうりゃく" 990 11 26)
     #(-30760333200 "長徳" "ちょうとく" 995 3 25)
     #(-30638595600 "長保" "ちょうほう" 999 2 1)
     #(-30464499600 "寛弘" "かんこう" 1004 8 8)
     #(-30196227600 "長和" "ちょうわ" 1013 2 8)
     #(-30061184400 "寛仁" "かんにん" 1017 5 21)
     #(-29940570000 "治安" "じあん" 1021 3 17)
     #(-29832397200 "万寿" "まんじゅ" 1024 8 19)
     #(-29706253200 "長元" "ちょうげん" 1028 8 18)
     #(-29431069200 "長暦" "ちょうりゃく" 1037 5 9)
     #(-29317194000 "長久" "ちょうきゅう" 1040 12 16)
     #(-29190963600 "寛徳" "かんとく" 1044 12 16)
     #(-29145862800 "永承" "えいしょう" 1046 5 22)
     #(-28934442000 "天喜" "てんぎ" 1053 2 2)
     #(-28756803600 "康平" "こうへい" 1058 9 19)
     #(-28537261200 "治暦" "じりゃく" 1065 9 4)
     #(-28421485200 "延久" "えんきゅう" 1069 5 6)
     #(-28252141200 "承保" "じょうほう" 1074 9 16)
     #(-28150621200 "承暦" "じょうりゃく" 1077 12 5)
     #(-28046682000 "永保" "えいほう" 1081 3 22)
     #(-27952506000 "応徳" "おうとく" 1084 3 15)
     #(-27852973200 "寛治" "かんじ" 1087 5 11)
     #(-27609843600 "嘉保" "かほう" 1095 1 23)
     #(-27548499600 "永長" "えいちょう" 1097 1 3)
     #(-27517568400 "承徳" "じょうとく" 1097 12 27)
     #(-27463309200 "康和" "こうわ" 1099 9 15)
     #(-27321958800 "長治" "ちょうじ" 1104 3 8)
     #(-27253184400 "嘉承" "かしょう" 1106 5 13)
     #(-27179744400 "天仁" "てんにん" 1108 9 9)
     #(-27120128400 "天永" "てんえい" 1110 7 31)
     #(-27023360400 "永久" "えいきゅう" 1113 8 25)
     #(-26876048400 "元永" "げんえい" 1118 4 25)
     #(-26811680400 "保安" "ほうあん" 1120 5 9)
     #(-26684672400 "天治" "てんじ" 1124 5 18)
     #(-26629549200 "大治" "だいじ" 1126 2 15)
     #(-26470659600 "天承" "てんしょう" 1131 2 28)
     #(-26421325200 "長承" "ちょうしょう" 1132 9 21)
     #(-26335616400 "保延" "ほうえん" 1135 6 10)
     #(-26140784400 "永治" "えいじ" 1141 8 13)
     #(-26116074000 "康治" "こうじ" 1142 5 25)
     #(-26057926800 "天養" "てんよう" 1144 3 28)
     #(-26014640400 "久安" "きゅうあん" 1145 8 12)
     #(-25840717200 "仁平" "にんぺい" 1151 2 14)
     #(-25720707600 "久寿" "きゅうじゅ" 1154 12 4)
     #(-25674829200 "保元" "ほうげん" 1156 5 18)
     #(-25580998800 "平治" "へいじ" 1159 5 9)
     #(-25556374800 "永暦" "えいりゃく" 1160 2 18)
     #(-25506003600 "応保" "おうほう" 1161 9 24)
     #(-25455200400 "長寛" "ちょうかん" 1163 5 4)
     #(-25385994000 "永万" "えいまん" 1165 7 14)
     #(-25348237200 "仁安" "にんあん" 1166 9 23)
     #(-25265725200 "嘉応" "かおう" 1169 5 6)
     #(-25200752400 "承安" "じょうあん" 1171 5 27)
     #(-25067523600 "安元" "あんげん" 1175 8 16)
     #(-25003328400 "治承" "じしょう" 1177 8 29)
     #(-24877443600 "養和" "ようわ" 1181 8 25)
     #(-24850746000 "寿永" "じゅえい" 1182 6 29)
     #(-24790438800 "元暦" "げんりゃく" 1184 5 27)
     #(-24749917200 "文治" "ぶんじ" 1185 9 9)
     #(-24602086800 "建久" "けんきゅう" 1190 5 16)
     #(-24317485200 "正治" "しょうじ" 1199 5 23)
     #(-24260029200 "建仁" "けんにん" 1201 3 19)
     #(-24164902800 "元久" "げんきゅう" 1204 3 23)
     #(-24095437200 "建永" "けんえい" 1206 6 5)
     #(-24049731600 "承元" "じょうげん" 1207 11 16)
     #(-23941386000 "建暦" "けんりゃく" 1211 4 23)
     #(-23854899600 "建保" "けんぽう" 1214 1 18)
     #(-23685987600 "承久" "じょうきゅう" 1219 5 27)
     #(-23591466000 "貞応" "じょうおう" 1222 5 25)
     #(-23509299600 "元仁" "げんにん" 1224 12 31)
     #(-23496598800 "嘉禄" "かろく" 1225 5 28)
     #(-23413136400 "安貞" "あんてい" 1228 1 18)
     #(-23375379600 "寛喜" "かんぎ" 1229 3 31)
     #(-23278611600 "貞永" "じょうえい" 1232 4 23)
     #(-23244397200 "天福" "てんぷく" 1233 5 25)
     #(-23196704400 "文暦" "ぶんりゃく" 1234 11 27)
     #(-23167414800 "嘉禎" "かてい" 1235 11 1)
     #(-23067622800 "暦仁" "りゃくにん" 1238 12 30)
     #(-23061315600 "延応" "えんおう" 1239 3 13)
     #(-23017165200 "仁治" "にんじ" 1240 8 5)
     #(-22934653200 "寛元" "かんげん" 1243 3 18)
     #(-22806867600 "宝治" "ほうじ" 1247 4 5)
     #(-22741462800 "建長" "けんちょう" 1249 5 2)
     #(-22505331600 "康元" "こうげん" 1256 10 24)
     #(-22491766800 "正嘉" "しょうか" 1257 3 31)
     #(-22426880400 "正元" "しょうげん" 1259 4 20)
     #(-22392320400 "文応" "ぶんおう" 1260 5 24)
     #(-22366314000 "弘長" "こうちょう" 1261 3 22)
     #(-22271101200 "文永" "ぶんえい" 1264 3 27)
     #(-21919194000 "建治" "けんじ" 1275 5 22)
     #(-21829683600 "弘安" "こうあん" 1278 3 23)
     #(-21508275600 "正応" "しょうおう" 1288 5 29)
     #(-21341955600 "永仁" "えいにん" 1293 9 6)
     #(-21161552400 "正安" "しょうあん" 1299 5 25)
     #(-21049664400 "乾元" "けんげん" 1302 12 10)
     #(-21025472400 "嘉元" "かげん" 1303 9 16)
     #(-20920064400 "徳治" "とくじ" 1307 1 18)
     #(-20861830800 "延慶" "えんきょう" 1308 11 22)
     #(-20783552400 "応長" "おうちょう" 1311 5 17)
     #(-20753658000 "正和" "しょうわ" 1312 4 27)
     #(-20599606800 "文保" "ぶんぽう" 1317 3 16)
     #(-20531005200 "元応" "げんおう" 1319 5 18)
     #(-20472858000 "元亨" "げんこう" 1321 3 22)
     #(-20354058000 "正中" "しょうちゅう" 1324 12 25)
     #(-20309216400 "嘉暦" "かりゃく" 1326 5 28)
     #(-20204499600 "元徳" "げんとく" 1329 9 22)
     #(-20142291600 "元弘" "げんこう" 1331 9 11)
     #(-20120259600 "正慶" "しょうけい" 1332 5 23)
     #(-20064013200 "建武" "けんむ" 1334 3 5)
     #(-19997658000 "延元" "えんげん" 1336 4 11)
     #(-19867626000 "興国" "こうこく" 1340 5 25)
     #(-19657587600 "正平" "しょうへい" 1347 1 20)
     #(-18913770000 "建徳" "けんとく" 1370 8 16)
     #(-18859856400 "文中" "ぶんちゅう" 1372 5 1)
     #(-18760410000 "天授" "てんじゅ" 1375 6 26)
     #(-18580784400 "弘和" "こうわ" 1381 3 6)
     #(-18479696400 "元中" "げんちゅう" 1384 5 18)
     #(-19918774800 "暦応" "りゃくおう" 1338 10 11)
     #(-19803949200 "康永" "こうえい" 1342 6 1)
     #(-19694912400 "貞和" "じょうわ" 1345 11 15)
     #(-19556499600 "観応" "かんのう" 1350 4 4)
     #(-19474851600 "文和" "ぶんな" 1352 11 4)
     #(-19364950800 "延文" "えんぶん" 1356 4 29)
     #(-19206838800 "康安" "こうあん" 1361 5 4)
     #(-19161392400 "貞治" "じょうじ" 1362 10 11)
     #(-18990838800 "応安" "おうあん" 1368 3 7)
     #(-18768099600 "永和" "えいわ" 1375 3 29)
     #(-18640918800 "康暦" "こうりゃく" 1379 4 9)
     #(-18579574800 "永徳" "えいとく" 1381 3 20)
     #(-18484880400 "至徳" "しとく" 1384 3 19)
     #(-18372992400 "嘉慶" "かけい" 1387 10 5)
     #(-18328237200 "康応" "こうおう" 1389 3 7)
     #(-18293504400 "明徳" "めいとく" 1390 4 12)
     #(-18157597200 "応永" "おうえい" 1394 8 2)
     #(-17089174800 "正長" "しょうちょう" 1428 6 10)
     #(-17047789200 "永享" "えいきょう" 1429 10 3)
     #(-16686982800 "嘉吉" "かきつ" 1441 3 10)
     #(-16593584400 "文安" "ぶんあん" 1444 2 23)
     #(-16420784400 "宝徳" "ほうとく" 1449 8 16)
     #(-16326522000 "享徳" "きょうとく" 1452 8 10)
     #(-16229581200 "康正" "こうしょう" 1455 9 6)
     #(-16163053200 "長禄" "ちょうろく" 1457 10 16)
     #(-16059027600 "寛正" "かんしょう" 1461 2 1)
     #(-15897632400 "文正" "ぶんしょう" 1466 3 14)
     #(-15863850000 "応仁" "おうにん" 1467 4 9)
     #(-15795594000 "文明" "ぶんめい" 1469 6 8)
     #(-15222157200 "長享" "ちょうきょう" 1487 8 9)
     #(-15155802000 "延徳" "えんとく" 1489 9 16)
     #(-15064045200 "明応" "めいおう" 1492 8 12)
     #(-14792835600 "文亀" "ぶんき" 1501 3 18)
     #(-14698227600 "永正" "えいしょう" 1504 3 16)
     #(-14145354000 "大永" "たいえい" 1521 9 23)
     #(-13926070800 "享禄" "きょうろく" 1528 9 3)
     #(-13800272400 "天文" "てんぶん" 1532 8 29)
     #(-13068464400 "弘治" "こうじ" 1555 11 7)
     #(-12993987600 "永禄" "えいろく" 1558 3 18)
     #(-12609248400 "元亀" "げんき" 1570 5 27)
     #(-12506864400 "天正" "てんしょう" 1573 8 25)
     #(-11896189200 "文禄" "ぶんろく" 1593 1 10)
     #(-11772032400 "慶長" "けいちょう" 1596 12 16)
     #(-11181402000 "元和" "げんな" 1615 9 5)
     #(-10909414800 "寛永" "かんえい" 1624 4 17)
     #(-10254934800 "正保" "しょうほう" 1645 1 13)
     #(-10152896400 "慶安" "けいあん" 1648 4 7)
     #(-10009731600 "承応" "じょうおう" 1652 10 20)
     #(-9928602000 "明暦" "めいれき" 1655 5 18)
     #(-9825699600 "万治" "まんじ" 1658 8 21)
     #(-9738781200 "寛文" "かんぶん" 1661 5 23)
     #(-9346352400 "延宝" "えんぽう" 1673 10 30)
     #(-9093027600 "天和" "てんな" 1681 11 9)
     #(-9017082000 "貞享" "じょうきょう" 1684 4 5)
     #(-8873485200 "元禄" "げんろく" 1688 10 23)
     #(-8384979600 "宝永" "ほうえい" 1704 4 16)
     #(-8159389200 "正徳" "しょうとく" 1711 6 11)
     #(-7996352400 "享保" "きょうほう" 1716 8 9)
     #(-7370643600 "元文" "げんぶん" 1736 6 7)
     #(-7217802000 "寛保" "かんぽう" 1741 4 12)
     #(-7123798800 "延享" "えんきょう" 1744 4 3)
     #(-6986854800 "寛延" "かんえん" 1748 8 5)
     #(-6881014800 "宝暦" "ほうれき" 1751 12 14)
     #(-6485043600 "明和" "めいわ" 1764 6 30)
     #(-6218586000 "安永" "あんえい" 1772 12 10)
     #(-5954461200 "天明" "てんめい" 1781 4 25)
     #(-5707616400 "寛政" "かんせい" 1789 2 19)
     #(-5326506000 "享和" "きょうわ" 1801 3 19)
     #(-5231466000 "文化" "ぶんか" 1804 3 22)
     #(-4784173200 "文政" "ぶんせい" 1818 5 26)
     #(-4384573200 "天保" "てんぽう" 1831 1 23)
     #(-3943933200 "弘化" "こうか" 1845 1 9)
     #(-3842067600 "嘉永" "かえい" 1848 4 1)
     #(-3627882000 "安政" "あんせい" 1855 1 15)
     #(-3462771600 "万延" "まんえん" 1860 4 8)
     #(-3432186000 "文久" "ぶんきゅう" 1861 3 29)
     #(-3337578000 "元治" "げんじ" 1864 3 27)
     #(-3303104400 "慶応" "けいおう" 1865 5 1)
     #(-3216704400 "明治" "めいじ" 1868 1 25)
     #(-1812099600 "大正" "たいしょう" 1912 7 30)
     #(-1357635600 "昭和" "しょうわ" 1926 12 25)
     #(600188400 "平成" "へいせい" 1989 1 8)
     #(1556636400 "令和" "れいわ" 2019 5 1)
     ))

(define max-nengo (- (vector-length japanese-nengo) 1))

(define (lookup-japanese-nengo instant)
  (if (>= instant (nengo-instant (vector-ref japanese-nengo max-nengo)))
      max-nengo
      ;; nengo[lo] <= instant < nengo[hi]
      (let lp ((lo 0) (hi max-nengo))
        (if (>= lo (- hi 1))
            lo
            (let ((mid (quotient (+ lo hi) 2)))
              (if (< instant (nengo-instant (vector-ref japanese-nengo mid)))
                  (lp lo mid)
                  (lp mid hi)))))))

(define (japanese-year->gregorian era year)
  (+ -1 year (nengo-start-year (vector-ref japanese-nengo era))))

(define (is-ja-leap-year? era year)
  (let ((year (japanese-year->gregorian era year)))
    (if (<= era 208) ;; 天正
        (is-julian-leap-year? year)
        (is-leap-year? year))))

(define (ja-month-day-upper-bound era year month)
  (case month
    ((2) (if (is-ja-leap-year? era year) 29 28))
    ((4 6 9 11) 30)
    (else 31)))

(define (ja-days-to-start-of-month era year month)
  (+ (vector-ref cumulative-days-to-start-of-month (- month 1))
     (if (and (> month 2) (is-ja-leap-year? era year))
         1
         0)))

(define (instant->japanese-time instant)
  (let* ((nengo-index (lookup-japanese-nengo instant))
         (nengo (vector-ref japanese-nengo nengo-index))
         (offset (nengo-instant nengo))
         (start-month (nengo-start-month nengo))
         (start-day (nengo-start-day nengo))
         (seconds (- instant offset (* 9 60 60))))  ;; JST
    (temporal-adjust
     (make-japanese-time nengo-index 1 start-month start-day)
     'day (exact (ceiling (/ seconds (* 24 60 60)))))))

(define (japanese-time->instant date)
  (let ((nengo (vector-ref japanese-nengo (ja-time-era date)))
        (year0 (japanese-year->gregorian (ja-time-era date) 0))
        (year (japanese-year->gregorian (ja-time-era date)
                                        (ja-time-year date))))
    (+ (nengo-instant nengo)
       (- (* 24 60 60 (ja-days-to-start-of-month (ja-time-era date)
                                                 1
                                                 (nengo-start-month nengo))))
       (- (* 24 60 60 (leap-years-from-epoch-to year0)))
       (- (* 24 60 60 (- (nengo-start-day nengo) 1)))
       (* 365 24 60 60 (- (ja-time-year date) 1))
       (* 24 60 60 (leap-years-from-epoch-to year))
       (* 24 60 60 (ja-days-to-start-of-month (ja-time-era date)
                                              (ja-time-year date)
                                              (ja-time-month date)))
       (* 24 60 60 (- (ja-time-day date) 1)))))

(define (japanese-era-max-year era)
  (cond
   ((= era 0) 0)
   ((= era max-nengo) +inf.0)
   (else
    (let ((nengo (vector-ref japanese-nengo era)))
      (+ 1
         (quotient (+ (- (nengo-instant (vector-ref japanese-nengo (+ era 1)))
                         (nengo-instant nengo))
                      (* 24 60 60
                         (ja-days-to-start-of-month era
                                                    1
                                                    (nengo-start-month nengo)))
                      (* 24 60 60 (nengo-start-day nengo)))
                   (* #e365.25 24 60 60)))))))

(define-chronology chronology:japan
  (record JapaneseTime)
  (constructor make-japanese-time)
  (predicate japanese-time?)
  (fields
   (era ja-time-era 0 max-nengo)
   (year ja-time-year -inf.0 +inf.0
         (lambda (era) (if (zero? era) -inf.0 1))
         japanese-era-max-year)
   (month ja-time-month 1 12)
   (day ja-time-day 1 28 #f ja-month-day-upper-bound)  ;; TODO: gregorian jump
   ;; (hour ja-time-hour 0 23)
   ;; (minute ja-time-minute 0 59)
   ;; (second ja-time-second 0 59)
   ;; (nanosecond ja-time-nanosecond 0 999999999)
   )
  (virtual
   ;; (day-of-week
   ;;  (lambda (t)
   ;;    (day-of-week (datetime-year t) (datetime-month t) (datetime-day t))))
   ;; (days-in-month
   ;;  (lambda (t)
   ;;    (month-day-upper-bound #f (datetime-year t) (datetime-month t))))
   ;; (julian-day
   ;;  (lambda (t)
   ;;    (gregorian->julian-day-number
   ;;     (datetime-year t) (datetime-month t) (datetime-day t))))
   ;; (week-of-year )
   ;; (local-time-offset )
   )
  (to-instant
   japanese-time->instant)
  (from-instant
   instant->japanese-time))
