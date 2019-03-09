(ns clj-playground.advent-code-2018-test
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as s])
  (:require [clojure.set :as sets])
  (:require [clojure.java.io :as io]))

(def day-1-input
  (io/resource "resources/advent2018/day-1-input.txt"))

(defn split-lines-as-int-seq
  [url-input]
  (map #(Integer/parseInt %) (apply list (s/split-lines (slurp url-input)))))

(def day-1-input-seq
  (split-lines-as-int-seq day-1-input))

(defn end-frequency
  [xs]
  (reduce + 0 xs))

;; seq of reductions
(defn freqs-reductions
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
  [url-input]
  (apply list (s/split-lines (slurp url-input))))

(def day-2-input-seq
  (split-lines-as-str-seq day-2-input))

;; count values = 2, = 3
;; juxt applies both fns at once

(defn or-between-sides
  [boolean-couples]
  (reduce
   (fn [m xs]
     {:first  (or (get m :first)  (xs 0))
      :second (or (get m :second) (xs 1))})
   {:first false :second false}
   boolean-couples))

(defn counts-2-3
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
 
(defn bool->int [b] (if b 1 0))

;;checksum function
(defn checksum
  [strings]
  (reduce * 1
          (vec-sum
           (map #(map bool->int (vals %))
                (map counts-2-3 strings)))))

(defn map-fn-on-map-vals [f m]
  (reduce (fn [altered-map [k v]]
            (assoc altered-map k (f v)))
          {}
          m))

;; values to 0-1
(defn values-to-zero-one
  [maps]
  (map #(map-fn-on-map-vals bool->int %) maps))

(defn different-at-idx
  [st1 st2]
  (keep-indexed (fn
                 [idx itm]
                 ;; idx if different, nil if same
                 (if (not= (itm 0) (itm 1)) idx))                          
               (map vector
                    (seq (char-array st1))
                    (seq (char-array st2)))))
;; it works until here

;; from https://gist.github.com/trhura/8131492
(defn combinations [lst k]
  (letfn [(combinator [x xs]
            (if (= (count x) k)
              [x]
              (when (not (empty? xs))
                (concat (combinator (concat x [(first xs)]) (rest xs))
                        (combinator x (rest xs))))))]
    (combinator nil lst)))

;; works

;; transform in a map :pair :differences
(defn strings-to-map-of-differences
  [strings size]
  (reduce (fn [m pair]
          (conj m {:pair pair
                   :differences (apply different-at-idx pair)}))
        '()
        (combinations strings size)))

(def maps-seq
  '({:pair 1 :differences '(0 1)}
    {:pair 2 :differences '(1)})) 

;; filter maps with :differences length 1
;; TODO fixme
(defn filter-with-diff-1
  [ms]
  (filter (fn [m]
            ;;(println (seq (get m :differences)))
            ;;(println (count (seq (get m :differences))))
            (= (count (get m :differences)) 1))
          ms))

;;(filter-with-diff-1 maps-seq)

(defn remove-char-at
  "remove char at x from string"
  [s idx]
  (apply str (remove #(= (get s idx) %) (seq s))))

(defn read-pair-and-remove
  [m]
  (remove-char-at (first (:pair m)) (first (:differences m))))

;;(read-pair-and-remove {:pair '("ac" "bc") :differences '(0)})

;; works

(deftest day-2-test
  (testing "day-2-tests-1"
    (is (= (count day-2-input-seq) 250))
    (is (= (counts-2-3 "abcdef") {:first false, :second false}))
    (is (= (counts-2-3 "bababc") {:first true,  :second true}))
    (is (= (counts-2-3 "abbcde") {:first true,  :second false}))
    (is (= (counts-2-3 "abcccd") {:first false, :second true}))
    (is (= (counts-2-3 "aabcdd") {:first true,  :second false}))
    (is (= (counts-2-3 "abcdee") {:first true,  :second false}))
    (is (= (counts-2-3 "ababab") {:first false, :second true}))
    ;; checksum
    (is (= (checksum ["abcdef"
                      "bababc"
                      "abbcde"
                      "abcccd"
                      "aabcdd"
                      "abcdee"
                      "ababab"]) 12))
    (is (= (checksum day-2-input-seq) 6474)))

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

  (defn find-common-letters
    [strings]
    (read-pair-and-remove
     (first (filter-with-diff-1
             (strings-to-map-of-differences strings 2)))))
  
  (testing "day-2-tests-2"
    (is (= 1 1))
    
    (is (= (different-at-idx "abcde" "axcye") [1 3]))
    (is (= (different-at-idx "fghij" "fguij") [2]))
    (is (= (remove-char-at "abc" 1) "ac"))
    (is (= (strings-to-map-of-differences simpler-test-input 2)
           '({:pair ("abcdefgh" "abcdefbh")
             :differences (6)})))

    (is (= (filter-with-diff-1
            '({:pair '("ab" "cd") :differences (0 1)}
              {:pair '("ac" "ad") :differences (1)}))
           '({:pair '("ac" "ad") :differences (1)})))
    
    (is (= (read-pair-and-remove {:pair '("abc" "adc") :differences '(1)})
           "ac"))

    (is (= (find-common-letters test-input) "fgij"))
         
    (is (= (find-common-letters day-2-input-seq)
        "mxhwoglxgeauywfkztncvjqr"))
    )

  )

