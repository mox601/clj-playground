(ns clj-playground.specter-test
  (:require [clojure.test :refer :all]
            [com.rpl.specter :refer :all]))

(def data {:a [{:aa 1 :bb 2}
               {:cc 3}]
           :b [{:dd 4}]})

(def inc-data {:a [{:aa 1 :bb 3}
                   {:cc 3}]
               :b [{:dd 5}]})

(deftest a-specter-test
  (testing "specter"
    (is (= (transform [MAP-VALS ALL MAP-VALS even?] inc data)
           inc-data))))
