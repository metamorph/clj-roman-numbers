(ns roman-numbers.core-test
  (:require [clojure.test :refer :all]
            [roman-numbers.core :refer :all]))

(deftest tools
  (testing "to-kw"
    (is (= [:X :I :V] (to-kw "xiv"))))
  (testing "drop-non-valid-digits"
    (is (= [:X :V] (filter valid-roman [:X :V :Z]) ))))

(deftest test-unpack
  (testing "subtractive forms"
    (is (= (str->roman "IIII") (unpack (str->roman "IV"))))))


(deftest addition
  (testing "simple addition"
    (is (= "II" (add-roman "I" "I")))
    (is (= "VI" (add-roman "V" "I"))))
  (testing "addition step 2"
    (is (= "VI" (add-roman "III" "III")))))
