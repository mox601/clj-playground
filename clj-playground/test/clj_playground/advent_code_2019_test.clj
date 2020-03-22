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
  [vs]
  (loop [pairs (cartesian-product (range 1 100))
         offset 0]
    (when (> (count pairs) offset)
      (let [[noun verb] (nth pairs offset)]
        (if (= 19690720 (calculate (program vs noun verb)))
          (mult-plus noun verb)
          (recur pairs (inc offset)))))))

;;works 5936 (calculate-until day-2-input-vec-ints 1 100)

(deftest day-2-test
  (testing "day-2"
    (is (= (count day-2-input-seq)
           165))
    (is (= (calculate (program day-2-input-vec-ints 12 2)))
        4462686)
    (is (= (calculate-until day-2-input-vec-ints)
           5936))))

(deftest day-3-test
  (testing "day-3"
    (is (= 1
           1))))


(def first-wire  "R8,U5,L5,D3")

(def second-wire "U7,R6,D4,L4")

;; distance is 3 + 3 = 6
(defn split-on-commas
  [s]
  (map #(s/split % #"") (s/split s #",")))

(split-on-commas first-wire)

;; [[0, 0] [0, 1], ... ]

;; intersections have same x and same y

;; given a string representation of a wire, return a list of points

(defn command-to-range
  [c end]
  (case c
    "R" (range  1 (inc end))
    "L" (range (dec (- 0 end)) 0)))

(command-to-range "L" 2)

;;; given [[0 0] ["R" "8"]] return [0 0] [1 0] [2 0]...[8 0]
(defn to-points
  [start-c command]
  (map (fn [s] (vec [(+ (nth start-c 1) s)
                     (nth start-c 0)]))
       (command-to-range (nth command 0)
                         (Integer/parseInt (nth command 1)))))
  
(to-points [0 0] ["R" "8"])
(to-points [0 1] ["R" "8"])

(to-points [0 0] ["L" "8"])
;; TODO handle ULD commands

;; each with its coordinates
;; iterate on first wire points and
;; for each point at x,y ,
;; test if the second wire has a point at the same coordinates x, y
;; if yes, add coordinates x, y to the list of the coordinates to return
;; if no, continue until end of first wire.

;; then, find the max manhattan distance of all the list of intersection coordinates and return it

