(ns clj-playground.advent-code-test
  (:require [clojure.test :refer :all]))

(defn captcha
  [x]
  (str x "a"))

(deftest day-1-test
  (testing "first digit (1) matches the second digit and the third digit (2) matches the fourth digit"
    (is (= (captcha "1122") 3)))
  (testing "each digit (all 1) matches the next")
  (testing "because no digit matches the next")
  (testing "the only digit that matches the next one is the last digit, 9."))
