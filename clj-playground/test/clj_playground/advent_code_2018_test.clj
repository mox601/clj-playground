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

(def m {\a 1 \b 2 \c 5})
(def xs '(\a \b \b \a \c))

;; values as set
(defn deduplicated-freqs
 [xs]
 (set (vals (frequencies xs))))

(deduplicated-freqs "abcc")

;; count values = 2, = 3
  
;; juxt applies both fns at once
;; just on set
(map (juxt (fn [x] (= 2 x)) (fn [x] (= 3 x))) #{1 2})

;; named fn
(defn list-of-couples
  [fset]
  (map (juxt
        (fn [x] (= 2 x))
        (fn [x] (= 3 x)))
       fset))

(list-of-couples #{1 2 3})

(defn or-between-sides
  [boolean-couples]
  (reduce
   (fn [m xs]
     {:first  (or (get m :first)  (xs 0))
      :second (or (get m :second) (xs 1))})
   {:first false :second false}
   boolean-couples))

(def boolean-couples
  [[false false] [true false] [false false]])

;; (deduplicated-freqs "abcc")
(defn counts-2-3
  [str]
  (or-between-sides
   (list-of-couples
    (deduplicated-freqs str))))

(defn map-fn-on-map-vals [f m]
  (reduce (fn [altered-map [k v]]
            (assoc altered-map k (f v)))
          {}
          m))

;; simpler, doesnt preserve structure

(map #(if % 1 0) (vals {:a true :b false}))

(def list-of-maps [{:a true :b false} {:a true :b true}])

(defn map-to-vals
  [ms]
  (map (fn [m] (map #(if % 1 0) (vals m)))
       ms))

  (defn vec-sum
    [pairs]
    (reduce #(mapv + %1 %2) pairs))
  
(vec-sum [[0 0] [1 0] [1 0] [1 0]])

;; works
(vec-sum (map-to-vals list-of-maps))

(vec-sum (map-to-vals (map counts-2-3 ["abbcde" "aabbb"])))

(reduce * 1 [2 3])

;;checksum function
(defn checksum
  [strings]
  (reduce * 1 (vec-sum (map-to-vals (map counts-2-3 strings)))))

(checksum ["abb" "aa" "aabbb" "accc"])
;;works

(defn bool->int [b] (if b 1 0))

;; values to 0-1
(def maps [{:a false :b true} {:a false :b false}])

(defn values-to-zero-one
  [maps]
  (map #(map-fn-on-map-vals bool->int %)
       maps))

  ;; it works
(deftest day-2-test
  (testing "day-2-tests"
    (is (= (count day-2-input-seq) 250))
    (is (= (or-between-sides boolean-couples) {:first true :second false}))
    (is (= (counts-2-3 "abcdef") {:first false, :second false}))
    (is (= (counts-2-3 "bababc") {:first true,  :second true}))
    (is (= (counts-2-3 "abbcde") {:first true,  :second false}))
    (is (= (counts-2-3 "abcccd") {:first false, :second true}))
    (is (= (counts-2-3 "aabcdd") {:first true,  :second false}))
    (is (= (counts-2-3 "abcdee") {:first true,  :second false}))
    (is (= (counts-2-3 "ababab") {:first false, :second true}))
    ;;checksum
    (is (= (checksum ["abcdef"
                      "bababc"
                      "abbcde"
                      "abcccd"
                      "aabcdd"
                      "abcdee"
                      "ababab"]) 12))
    (is (= (checksum day-2-input-seq) 6474))
        ))


