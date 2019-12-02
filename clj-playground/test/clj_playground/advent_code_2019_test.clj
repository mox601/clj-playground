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
 (loop [x m
       xs []]
  (if (< x 0)
    xs
    (recur (f x) (conj xs (f x))))))

(defn sum-of-map-fn
  [fn ms]
  (reduce + 0 (map fn ms)))

(deftest day-1-test  
  (testing "day-1-functions"
    (is (= (sum-of-map-fn fuel-requirement day-1-input-seq)) 3369286)
    (is (= (sum-of-map-fn #(reduce + 0 (drop-last (loop-until-negative fuel-requirement %)))  day-1-input-seq) 5051054))))


