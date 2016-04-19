(ns roman-numbers.core-test
  (:require [clojure.test :refer :all]
            [roman-numbers.core :refer :all]))

(deftest verify-validator
  (testing "handles lower-case"
    (is (= "XIV" (str->roman "XIV")))
    (is (= "XIV" (str->roman "xiv")))
    (is (= "XIV" (str->roman "x I v  "))))
  (testing "fails on illegal input"
    (is (thrown? AssertionError (str->roman "xyb")))))
