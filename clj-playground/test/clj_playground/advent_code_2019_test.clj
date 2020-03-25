(ns clj-playground.advent-code-2019-test
  (:require [clojure.test    :refer :all])
  (:require [clojure.string  :as s])
  (:require [clojure.java.io :as io])
  (:require [clojure.set     :as sets]))

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


(def first-wire  "R8,U5,L5,D300")

(def second-wire "U7,R6,D4,L4")

;; distance is 3 + 3 = 6
(defn split-on-commas
  [s]
  (map #(vec [(subs % 0 1) (subs % 1)]) (s/split s #",")))

(split-on-commas first-wire)

;; intersections have same x and same y

;; given a string representation of a wire, return a list of points

(defn direction-to-range
  [direction distance]
  (case direction
    ("R" "U") (range  1 (inc distance))
    ("L" "D") (map #(- 0 %) (range 1 (inc distance)))))

(direction-to-range "R" 2) ;; 1 2
(direction-to-range "L" 2) ;; -1 -2
(direction-to-range "U" 2)
(direction-to-range "D" 2)

(defn dir-to-vec
  [direction x-start y-start s]
  (case direction
    ("R" "L") (vec [(+ x-start s) y-start])
    ("U" "D") (vec [x-start (+ y-start s)])))

(dir-to-vec "R" 0 0 3)
(dir-to-vec "D" 0 0 3)

;;; given [[0 0] ["R" "8"]] return [0 0] [1 0] [2 0]...[8 0]
(defn to-segment
  [start-c command]
  (let [x-start (nth start-c 0)
        y-start (nth start-c 1)
        direction (nth command 0)
        distance  (Integer/parseInt (nth command 1))]
    (vec (map (fn [s]  (dir-to-vec direction x-start y-start s))
              (direction-to-range direction distance)))))

(to-segment [0 0] ["R" "2"])
(to-segment [0 1] ["R" "2"])
(to-segment [0 0] ["L" "2"])
(to-segment [0 0] ["U" "2"])
(to-segment [0 0] ["D" "2"])
;;ok

;; loop [starting-point input-wire output-wire]
;; use last point of previous segment to start next segment
;; recur new starting-point

(defn commands-to-segments
  [cmds]
  (loop [starting-point [0 0]
         input-commands cmds
         offset 0
         output-wire []]
    (if (> (count input-commands) offset)
      (let [segment (to-segment starting-point (nth input-commands offset))]
        (recur (last segment) input-commands (inc offset) (conj output-wire segment)))
      (apply concat output-wire))))

(def test-wire [["R" "2"] ["U" "2"]])
(commands-to-segments [["R" "2"] ["U" "2"]])
(commands-to-segments [["R" "2"] ["D" "2"] ["L" "2"] ["U" "2"]])
;;ok


(sets/intersection
 (set (commands-to-segments [["R" "2"] ["U" "2"]]))
 (set (commands-to-segments [["U" "2"] ["R" "2"]]))
 (set (commands-to-segments [["U" "3"] ["R" "2"] ["D" "1"]])))
;; #{[2 2]}

;;ok!

;; [[0, 0] [0, 1], ... ]


;; each with its coordinates
;; iterate on first wire points and
;; for each point at x,y ,
;; test if the second wire has a point at the same coordinates x, y
;; if yes, add coordinates x, y to the list of the coordinates to return
;; if no, continue until end of first wire.


(defn manhattan-distance
  [point]
    (+ (Math/abs (nth point 0))
       (Math/abs (nth point 1))))

(manhattan-distance [0 0])
(manhattan-distance [1 1])
(manhattan-distance [1 -1])
(manhattan-distance [-1 -1])

;; then, find the max manhattan distance of all the list of intersection coordinates and return it
(defn min-manhattan-distance
  [points]
  (apply min (map manhattan-distance points)))

(min-manhattan-distance [[1 0] [0 1] [3 3] [-3 -4]])

;; read file day-3.txt 
(def day-3-input
  (io/resource "resources/advent2019/day-3.txt"))
;; split on new lines

(defn res-to-commands
  ""
  [url-input]
  (map split-on-commas (s/split-lines (slurp url-input))))

(def first-wire-input (nth (res-to-commands day-3-input) 0))

(def second-wire-input (nth (res-to-commands day-3-input) 1))

(commands-to-segments first-wire-input)

(def intersections
  (sets/intersection
 (set (commands-to-segments first-wire-input))
 (set (commands-to-segments second-wire-input))))


(min-manhattan-distance intersections)
;;865
;;OK!


