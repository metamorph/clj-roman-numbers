(ns roman-numbers.core-test
  (:require [clojure.test :refer :all]
            [roman-numbers.core :refer :all]))

(deftest tools
  (testing "to-kw"
    (is (= [:X :I :V] (to-kw "xiv"))))
  (testing "drop-non-valid-digits"
    (is (= [:X :V] (filter valid-roman [:X :V :Z])))))

(deftest test-unpack
  (testing "subtractive forms"
    (is (= (str->roman "IIII") (unpack (str->roman "IV"))))))

(deftest test-sorting
  (testing "simple sort"
    (is (= (str->roman "xvii") (roman-sort (str->roman "ixvi"))))))

(deftest addition
  (testing "simple addition"
    (is (= "II" (add-roman "I" "I")))
    (is (= "VI" (add-roman "V" "I"))))
  (testing "addition step 2"
    (is (= "VI" (add-roman "III" "III")))))

(deftest test-map-and-replace

  (testing "noop/identity"
    (letfn [(map-identity [col] [[(first col)] 1])]
      (is (= [1 2 3 4] (map-and-replace [1 2 3 4] map-identity)))))

  (testing "duplictate even"
    (letfn [(double-even [col]
              (let [f (first col)]
                (if (even? f)
                  [[f f] 1]
                  [[f] 1])))]
      (is (= [1 2 2 3 4 4] (map-and-replace [1 2 3 4] double-even))))))
