(ns clj-playground.advent-code-2018-test
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as s])
  (:require [clojure.set :as sets])
  (:require [clojure.java.io :as io]))

(def day-1-input
  (io/resource "resources/advent2018/day-1-input.txt"))

(defn split-lines-as-int-seq
  ""
  [url-input]
  (map #(Integer/parseInt %) (apply list (s/split-lines (slurp url-input)))))
 
(def day-1-input-seq
  (split-lines-as-int-seq day-1-input))

(defn end-frequency
  ""
  [xs]
  (reduce + 0 xs))

(defn freqs-reductions
  "seq of reductions"
  [xs]
  (reductions + 0 xs))

(defn first-duplicate
  [xs]
  (reduce (fn [m v]
            (if (nil? (get m v))
              (assoc m v 1)
              (reduced v)))
          {} xs))

(deftest day-1-test
  (testing "day-1-tests"
    (is (= (count day-1-input-seq) 1023))
    (is (= (end-frequency day-1-input-seq) 590))
    (is (= (first-duplicate (freqs-reductions (cycle day-1-input-seq))) 83445))))

(def day-2-input
  (io/resource "resources/advent2018/day-2-input.txt"))

(defn split-lines-as-str-seq
  ""
  [url-input]
  (apply list (s/split-lines (slurp url-input))))

(def day-2-input-seq
  (split-lines-as-str-seq day-2-input))

;; count values = 2, = 3
;; juxt applies both fns at once
(defn or-between-sides
  ""
  [boolean-couples]
  (reduce
   (fn [m xs]
     {:first  (or (get m :first)  (xs 0))
      :second (or (get m :second) (xs 1))})
   {:first false :second false}
   boolean-couples))

(defn counts-2-3
  ""
  [str]
  (or-between-sides
   (map (juxt
        (fn [x] (= 2 x))
        (fn [x] (= 3 x)))
       (set (vals (frequencies str))))))

;; simpler, doesnt preserve structure
;; (map #(if % 1 0) (vals {:a true :b false}))
(defn vec-sum
  [pairs]
  (reduce #(mapv + %1 %2) pairs))
 
(defn bool->int
  ""
  [b]
  (if b 1 0))

(defn checksum
  ""
  [strings]
  (reduce * 1
          (vec-sum
           (map #(map bool->int (vals %))
                (map counts-2-3 strings)))))

(defn map-fn-on-map-vals
  ""
  [f m]
  (reduce (fn [altered-map [k v]]
            (assoc altered-map k (f v)))
          {}
          m))

(defn values-to-zero-one
  "values to 0-1"
  [maps]
  (map #(map-fn-on-map-vals bool->int %) maps))

(defn different-at-idx
  ""
  [st1 st2]
  (keep-indexed (fn
                 [idx itm]
                 ;; idx if different, nil if same
                 (if (not= (itm 0) (itm 1)) idx))                          
               (map vector
                    (seq (char-array st1))
                    (seq (char-array st2)))))

(defn combinations
  "from https://gist.github.com/trhura/8131492"
  [lst k]
  (letfn [(combinator [x xs]
            (if (= (count x) k)
              [x]
              (when (not (empty? xs))
                (concat (combinator (concat x [(first xs)]) (rest xs))
                        (combinator x (rest xs))))))]
    (combinator nil lst)))

(defn strings-to-map-of-differences
  "transform in a map :pair :differences"
  [strings size]
  (reduce (fn [m pair]
            (conj m {:pair        pair
                     :differences (apply different-at-idx pair)}))
        '()
        (combinations strings size)))

(def maps-seq
  '({:pair 1 :differences (0 1)}
    {:pair 2 :differences (1)})) 

;; filter maps with :differences length 1

(defn filter-with-diff-1
  ""
  [ms]
  (filter (fn [m]
            (= 1 (count (get m :differences))))
          ms))

;;(filter-with-diff-1 maps-seq)
;; broken!
;; fixed thanks to teraflop https://www.reddit.com/r/adventofcode/comments/azec8t/2018_day_2_part_2_clojure_misunderstood/ei7zrc8/
(defn remove-char-at
  "remove char at x from string"
  [s idx]
  (apply str (keep-indexed (fn [index item]
                             (if (not= idx index)
                               item
                               nil))
                           (seq s))))

(defn read-pair-and-remove
  ""
  [m]
  (remove-char-at (first (:pair m)) (first (:differences m))))

(defn find-common-letters
  ""
  [strings]
  (read-pair-and-remove
   (first (filter-with-diff-1
           (strings-to-map-of-differences strings 2)))))

(def test-input
  '("abcde"
    "fghij"
    "klmno"
    "pqrst"
    "fguij"
    "axcye"
    "wvxyz"))

(def simpler-test-input
  '("abcdefgh"
    "abcdefbh"))

(def test-input-from-reddit
  '("abcdefgh"
    "abcdefbh"
    "abcdqrst"
    "hjklmnop"))

(deftest day-2-test

  (testing "day-2-1-functions"
    (is (= (count day-2-input-seq) 250))
    (is (= (counts-2-3 "abcdef") {:first false, :second false}))
    (is (= (counts-2-3 "bababc") {:first true, :second true}))
    (is (= (counts-2-3 "abbcde") {:first true, :second false}))
    (is (= (counts-2-3 "abcccd") {:first false, :second true}))
    (is (= (counts-2-3 "aabcdd") {:first true, :second false}))
    (is (= (counts-2-3 "abcdee") {:first true, :second false}))
    (is (= (counts-2-3 "ababab") {:first false, :second true}))
    (is (= (checksum ["abcdef"
                      "bababc"
                      "abbcde"
                      "abcccd"
                      "aabcdd"
                      "abcdee"
                      "ababab"]) 12)))
  
  (testing "day-2-tests-1"
    (is (= (checksum day-2-input-seq) 6474)))

  (testing "day-2-2-functions"
    (is (= (different-at-idx "abcde" "axcye") [1 3]))
    (is (= (different-at-idx "fghij" "fguij") [2]))

    (is (= (remove-char-at "aa" 0) "a"))
    (is (= (remove-char-at "aa" 1) "a"))
    (is (= (remove-char-at "aa" 2) "aa"))
    
    (is (= (remove-char-at "abc" 0) "bc"))
    (is (= (remove-char-at "abc" 1) "ac"))
    (is (= (remove-char-at "abc" 2) "ab"))

    (is (= (strings-to-map-of-differences simpler-test-input 2)
           '({:pair        ("abcdefgh" "abcdefbh")
              :differences (6)})))
    (is (= (strings-to-map-of-differences '("aa" "ab") 2)
           '({:pair ("aa" "ab")
              :differences (1)})))

    (is (= (first
            (filter-with-diff-1 (strings-to-map-of-differences '("aa" "ab") 2)))
           {:pair '("aa" "ab")
            :differences '(1)}))

    (is (= (read-pair-and-remove {:pair '("aa" "ab")
                                  :differences '(1)})
           "a"))
    
    (is (= (combinations '(:a :b :c) 2) '((:a :b) (:a :c) (:b :c))) )
    
    (is (= (filter-with-diff-1
            '({:pair '("ab" "cd") :differences (0 1)}
              {:pair '("ac" "ad") :differences (1)}))
           '({:pair '("ac" "ad") :differences (1)})))

    (is (= (read-pair-and-remove {:pair '("abc" "adc") :differences '(1)})
           "ac"))

    (is (= (find-common-letters test-input-from-reddit) "abcdefh"))
    (is (= (find-common-letters test-input) "fgij"))
    
    (is (= (filter-with-diff-1
            (strings-to-map-of-differences day-2-input-seq 2))
           '({:pair ("mxhwoglxgeauywfdkztndcvjqr" "mxhwoglxgeauywfikztndcvjqr") :differences (15)})))) 

  (testing "day-2-tests-2"
    (is (= (find-common-letters day-2-input-seq) "mxhwoglxgeauywfkztndcvjqr"))))


(def day-3-input
  (io/resource "resources/advent2018/day-3-input.txt"))

(defn split-lines-as-str-seq
  ""
  [url-input]
  (apply list (s/split-lines (slurp url-input))))

(def day-3-input-seq
  (split-lines-as-str-seq day-3-input))

(defn split-on-char-take-nth
  [str regx n]
  (nth (s/split str regx) n))

(split-on-char-take-nth "18x25:" #"x" 0)

;; smaller functions
;; 836,706:
(defn str->left-top
  [str]
  (let [split-str (s/split str #",")]  
    {:left (Integer/parseInt (nth split-str 0))
     :top  (Integer/parseInt (s/replace (nth split-str 1) #":" ""))}))

(defn str->id
  [str]
  {:id (Integer/parseInt
   (s/replace str #"#" ""))})

;; works
;; 18x25
(defn str->width-height
  [str]
  (let [split-str  (s/split str #"x")]
    {:width  (Integer/parseInt (nth split-str 0))
     :height (Integer/parseInt (nth split-str 1))}))

(str->id (split-on-char-take-nth "#19 @ 836,706: 18x25" #" " 0))
(str->left-top (split-on-char-take-nth "#19 @ 836,706: 18x25" #" " 2))
(str->width-height (split-on-char-take-nth "#19 @ 836,706: 18x25" #" " 3))
;; works

;;  parse and transform a string like
;; "#19 @ 836,706: 18x25"
;; to a map :id :left :top :width :height
;; merging N maps:
(defn str->map
  [str]
  ;; split just once
  (let [split-str (s/split str #" ")]
    (merge
     (str->id           (nth split-str 0))
     (str->left-top     (nth split-str 2))
     (str->width-height (nth split-str 3)))))

(str->map "#19 @ 836,706: 18x25")
;; works

;; refactored as smaller functions returning maps and then merge
;;commented out
(comment (defn str->map
  [str]
  (let [id (Integer/parseInt
            (s/replace (split-on-char-take-nth str #" " 0) #"#" ""))
        l  (Integer/parseInt
            (split-on-char-take-nth (s/replace (nth (s/split str #" ") 2) #":" "")
                                    #"," 0))
        t  (Integer/parseInt (split-on-char-take-nth
                              (s/replace (nth (s/split str #" ") 2) #":" "")
                              #"," 1))
        w  (Integer/parseInt (split-on-char-take-nth
                              (nth (s/split str #" ") 3)
                              #"x" 0))
        h  (Integer/parseInt(split-on-char-take-nth (nth (s/split str #" ") 3) #"x" 1))]
    {:id id
     :left l
     :top t
     :width w
     :height h})))

(def claims-test
  [{:id 1 :left 1 :top 3 :width 4 :height 4}
   {:id 2 :left 3 :top 1 :width 4 :height 4}
   {:id 3 :left 5 :top 5 :width 2 :height 2}])

(defn to-empty-matrix
  [m]
  (let [rows (+ (get m :top) (get m :height))
        cols (+ (get m :left) (get m :width))]
    (vec (repeat rows (vec (repeat cols 0))))))

;;returns row x columns dimensions of enclosing matrix
(to-empty-matrix {:id 2 :left 3 :top 1 :width 4 :height 4})



(defn print-matrix
  [mat]
  (map #(println %)  mat))

(print-matrix [[1 1 0] [1 1 0]])


(deftest day-3-test
  (testing "day-3-1-functions"
    (is (= (count day-2-input-seq) 250))
    (is (= (str->map "#19 @ 836,706: 18x25")
           {:id 19 :left 836 :top 706 :width 18 :height 25}))

    )
  (testing "day-3-tests-2"
    (is (= (count day-2-input-seq) 250))))


