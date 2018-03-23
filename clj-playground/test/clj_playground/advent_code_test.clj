(ns clj-playground.advent-code-test
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as s]))

(defn string-to-int-seq
  "converts a string to a sequence of integers"
  [str]
  (map #(Character/digit % 10) (seq str)))

(defn couples-from
  "returns a lazy sequence of the couples made from an element and his next (wrapping around the end of the sequence)"
  [xs]
  (take (count xs) (partition 2 1 (cycle xs))))

(defn first-when-all-equal
  "Returns the first if all the elements are the same, zero otherwise"
  [xs]
  (if (apply = xs) (first xs) 0))

(defn captcha
  "captcha"
  [str]
  (reduce + 0 (map #(first-when-all-equal %) (couples-from (string-to-int-seq str)))))

(captcha "181445682966897848665963472661939865313976877194312684993521259486517527961396717561854825453963181134379574918373213732184697746668399631642622373684425326112585283946462323363991753895647177797691214784149215198715986947573668987188746878678399624533792551651335979847131975965677957755571358934665327487287312467771187981424785514785421781781976477326712674311994735947987383516699897916595433228294198759715959469578766739518475118771755787196238772345762941477359483456641194685333528329581113788599843621326313592354167846466415943566183192946217689936174884493199368681514958669615226362538622898367728662941275658917124167353496334664239539753835439929664552886538885727235662548783529353611441231681613535447417941911479391558481443933134283852879511395429489152435996669232681215627723723565872291296878528334773391626672491878762288953597499218397146685679387438634857358552943964839321464529237533868734473777756775687759355878519113426969197211824325893376812556798483325994128743242544899625215765851923959798197562831313891371735973761384464685316273343541852758525318144681364492173465174512856618292785483181956548813344752352933634979165667651165776587656468598791994573513652324764687515345959621493346623821965554755615219855842969932269414839446887613738174567989512857785566352285988991946436148652839391593178736624957214917527759574235133666461988355855613377789115472297915429318142824465141688559333787512328799783539285826471818279818457674417354335454395644435889386297695625378256613558911695145397779576526397241795181294322797687168326696497256684943829666672341162656479563522892141714998477865114944671225898297338685958644728534192317628618817551492975251364233974374724968483637518876583946828819994321129556511537619253381981544394112184655586964655164192552352534626295996968762388827294873362719636616182786976922445125551927969267591395292198155775434997827738862786341543524544822321112131815475829945625787561369956264826651461575948462782869972654343749617939132353399334744265286151177931594514857563664329299713436914721119746932159456287267887878779218815883191236858656959258484139254446341")

(deftest day-1-test
  (testing "first digit (1) matches the second digit and the third digit (2) matches the fourth digit"
    (is (= (captcha "1122") 3)))
  (testing "each digit (all 1) matches the next"
    (is (= (captcha "1111") 4)))
  (testing "because no digit matches the next"
    (is (= (captcha "1234") 0)))
  (testing "the only digit that matches the next one is the last digit, 9."
    (is (= (captcha "91212129") 9))))

(defn split-lines-and-items
  [x]
  (map #(s/split % #"\s+") (s/split-lines x)))

(defn string-coll-to-int
  [strs]
  (map #(Integer/parseInt %) strs))

(defn string-matrix-to-int
  [x]
  (map string-coll-to-int x))

(defn max-and-min
  [x]
  [(apply max x) (apply min x)])

(defn difference-between-max-and-min
  [x]
  (apply - (max-and-min x)))

(defn difference-between-max-and-min-tbl
  [x]
  (reduce + 0 (map difference-between-max-and-min x)))

;;(def x "1\t2\n3\t4")
(defn checksum
  [x]
  (difference-between-max-and-min-tbl
            (string-matrix-to-int (split-lines-and-items x))))

(deftest day-2-test
  (testing
      (is (= (difference-between-max-and-min [5 1 9 5]) 8)))
  (testing
      (is (= (difference-between-max-and-min [7 5 3]) 4)))
  (testing
      (is (= (difference-between-max-and-min [2 4 6 8]) 6)))
  (testing
      (is (= (checksum "5 1 9 5
7 5 3
2 4 6 8") 18)))
  (testing
      (is (= (checksum "121	59	141	21	120	67	58	49	22	46	56	112	53	111	104	130
1926	1910	760	2055	28	2242	146	1485	163	976	1842	1982	137	1387	162	789
4088	258	2060	1014	4420	177	4159	194	2794	4673	4092	681	174	2924	170	3548
191	407	253	192	207	425	580	231	197	382	404	472	164	571	500	216
4700	1161	168	5398	5227	5119	252	2552	4887	5060	1152	3297	847	4525	220	262
2417	992	1445	184	554	2940	209	2574	2262	1911	2923	204	2273	2760	506	157
644	155	638	78	385	408	152	360	588	618	313	126	172	220	217	161
227	1047	117	500	1445	222	29	913	190	791	230	1281	1385	226	856	1380
436	46	141	545	122	86	283	124	249	511	347	502	168	468	117	94
2949	3286	2492	2145	1615	159	663	1158	154	939	166	2867	141	324	2862	641
1394	151	90	548	767	1572	150	913	141	1646	154	1351	1506	1510	707	400
646	178	1228	1229	270	167	161	1134	193	1312	1428	131	1457	719	1288	989
1108	1042	93	140	822	124	1037	1075	125	941	1125	298	136	94	135	711
112	2429	1987	2129	2557	1827	477	100	78	634	352	1637	588	77	1624	2500
514	218	209	185	197	137	393	555	588	569	710	537	48	309	519	138
1567	3246	4194	151	3112	903	1575	134	150	4184	3718	4077	180	4307	4097	1705") 32121))))

;; elements in each perimeter: 1, 8, 16, ...
;; elements in each perimeter side: 1, 3, 5, ...  
;; max number in perimeter: 1, 9, 25, ...
(defn max-number-in-perimeter
  "given the side of a perimeter, it returns the max number found in that perimeter. can only accept odd numbers"
  [n]
  (* n n))

;; mathematically, the first power of an odd number that is greater than n
(defn perimeter
  "given a number, returns the dimension of the side of the perimeter it belongs to.
  can only return odd numbers"
  [n]
  (if (= 1 n)
    1
    (+ 2
       (int (Math/sqrt (last (take-while #(> n %) (map max-number-in-perimeter (iterate #(+ 2 %) 1)))))))))

(defn take-nth-offset
  "given a number, returns the index to take from the offsets"
  [n]
  (- n (inc (max-number-in-perimeter (- (perimeter n) 2)))))

;;5 is the 3rd odd number
;;7 is the 4th odd number, and so on
(defn position-of-odd
  "given an odd number (the size of the side),
  return its position in the sequence of all odd numbers"
  [n]
  (+ 1 (/ (- n 1) 2)))

(defn moves-on-axes
  "given a number, return the number of moves needed from the perimeter it belongs to
  towards the centre"
  [n]
  (- (position-of-odd (perimeter n)) 1))

;(def moves
;  '(:right :up :left :down :right))

;(defn quantities
;  "side is odd"
;  [side]
;  (reverse [(dec side) (dec side) (dec side) (- side 2) 1]))

;(defn moves-in-square
;  "side is odd"
;  [side]
;  (flatten (map repeat (quantities side) moves)))

(defn offsets
  "side is odd
  given a side of a perimeter, build the sequence of offsets.
  An element of the sequence is the amount of moves needed
  to move towards the closest intersection between central axes"
  [side]
  (flatten
   (repeat 4 (concat
              (reverse (range 1 (quot side 2)))
              (range 0 (inc (quot side 2)))))))

(defn moves-towards-axes
  "given a number, return the number of lateral moves
  needed to move towards the horizontal axes"
  [n]
  (nth (offsets (perimeter n)) (take-nth-offset n)))

(defn manhattan-distance-to-centre
  [n]
  (if (= 1 n)
    0
    (+ (moves-on-axes n) (moves-towards-axes n))))

(deftest day-3-test

  (testing
      (is (= (max-number-in-perimeter 1) 1)))
  (testing
      (is (= (max-number-in-perimeter 3) 9)))
  (testing
      (is (= (max-number-in-perimeter 5) 25)))

  (testing
      (is (= (nth (offsets 3) 3) 1)))
  (testing
      (is (= (nth (offsets 3) 4) 0)))
  (testing
      (is (= (nth (offsets 3) 5) 1)))

  (testing
      (is (= (nth (offsets 5) 13)) 0))
  (testing
      (is (= (nth (offsets 5) 14)) 1))
  (testing
      (is (= (nth (offsets 5) 15)) 2))
  (testing
      (is (= (manhattan-distance-to-centre 1) 0)))
  (testing
      (is (= (manhattan-distance-to-centre 12) 3)))
  (testing
      (is (= (manhattan-distance-to-centre 23) 2)))
  (testing
      (is (= (manhattan-distance-to-centre 1024) 31)))
  (testing
      (is (= (manhattan-distance-to-centre 325489) 552)))
  )


