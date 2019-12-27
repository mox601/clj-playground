(ns clj-playground.advent-code-2019-test
  (:require [clojure.test    :refer :all])
  (:require [clojure.string  :as s])
  (:require [clojure.java.io :as io]))

(def day-1-input
  (io/resource "resources/advent2019/day-1.txt"))

(defn split-lines-as-int-seq
  ""
  [url-input]
  (map #(Integer/parseInt %) (apply list (s/split-lines (slurp url-input)))))

(def day-1-input-seq
  (split-lines-as-int-seq day-1-input))

(defn fuel-requirement
  [m]
  (- (quot m 3) 2))

(defn loop-until-negative
  [f m]
  (loop [x  m
         xs []]
    (if (< x 0)
      xs
      (recur (f x) (conj xs (f x))))))

(defn sum-of-map-fn
  [fn ms]
  (reduce + 0 (map fn ms)))

(deftest day-1-test  
  (testing "day-1"
    (is (= (sum-of-map-fn fuel-requirement day-1-input-seq)
           3369286))
    (is (= (sum-of-map-fn #(reduce + 0 (drop-last (loop-until-negative fuel-requirement %))) day-1-input-seq)
           5051054))))

(def day-2-input
  (slurp (io/resource "resources/advent2019/day-2.txt")))

(def day-2-input-seq (s/split day-2-input #"[,\n]"))

(def day-2-input-vec-ints
  (vec (map #(Integer/parseInt %) day-2-input-seq)))

(def memory day-2-input-vec-ints)

(defn program
  [mem noun verb]
  (assoc mem 1 noun 2 verb))

;; opcode 1 2 99
(defn opcode [n]
  (if (= 1 n)
    +
    (when (= 2 n)
      *)))

(defn next-idx
  [n]
  (+ 4 n))

(defn process-opcode
  [v offset]
  (let [[w x y z] (drop offset v)]
    (assoc v z ((opcode w)
                (nth v x)
                (nth v y)))))

(defn compute
  [vs]
  (loop [v      vs
         offset 0]
    (if (= 99 (nth v offset))
      v
      (recur (process-opcode v offset) (next-idx offset)))))

;; determine what pair of inputs produces the output 19690720

(defn calculate
  [ints]
  (first (compute ints)))

(defn mult-plus
  [noun verb]
  (+ (* 100 noun) verb))

(defn cartesian-product
  [s]
  (for [x s
        y s]
    (vector x y)))

;; works
;; calculate program with input cartesian product
;; until output is 19690720

(defn calculate-until
  [vs start end]
  (loop [pairs (cartesian-product (range start end))
         offset 0]
    (when (> (count pairs) offset)
      (let [[noun verb] (nth pairs offset)]
        (if (= 19690720 (calculate (program vs noun verb)))
          (mult-plus noun verb)
          (recur pairs (inc offset)))))))

;;works? 5936 (calculate-until day-2-input-vec-ints 1 100)

(deftest day-2-test
  (testing "day-2"
    (is (= (count day-2-input-seq) 165))
    (is (= (calculate (program day-2-input-vec-ints 12 2))) 4462686)
    (is (= (calculate-until day-2-input-vec-ints 1 100) 5936))))


