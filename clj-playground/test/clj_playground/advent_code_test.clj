(ns clj-playground.advent-code-test
  (:require [clojure.test :refer :all]))

(defn captcha
  [x]
  (str x "a"))

(deftest day-1-test
  (testing "fixed, I succeed."
    (is (= (captcha "") "a"))))
