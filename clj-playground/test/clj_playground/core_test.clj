(ns clj-playground.core-test
  (:require [clojure.test :refer :all]
            [clj-playground.core :refer :all]
            [clojure.spec.alpha :as s]))

(deftest a-test
  (testing "fixed, I succeed."
    (is (= 1 1))))
