(ns roman-numbers.core
  "Functions for working with Roman numerals. The algorithms for addition etc are
  taken from here: http://turner.faculty.swau.edu/mathematics/materialslibrary/roman/"
  (:require [clojure.string :as str]))

(def valid-roman-chars [\M \L \C \D \X \V \I])
(def subtractive-forms {"IV" "IIII"
                         "IX" "VIIII"
                         "XL" "XXXX"
                         "XC" "LXXXX"
                         "CD" "CCCC"
                         "CM" "DCCCC"})

(defn str->roman
  "Convert string to a valid roman number.
  Asserts that the string doesn't contain any ugly chars.
  Lower case chars will be upper-cased"
  [s]
  {:post [(empty? (clojure.set/difference
                   (set %)
                   (apply hash-set valid-roman-chars)))]}
  (->> (filter #(not (= \space %)) s)
       (map str/upper-case)
       (apply str)))
