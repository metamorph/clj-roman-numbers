(ns roman-numbers.core
  "Functions for working with Roman numerals. The algorithms for addition etc are
  taken from here: http://turner.faculty.swau.edu/mathematics/materialslibrary/roman/"
  (:require [clojure.string :as str]))

(def *valid-roman-digits* [:I :V :X :L :C :D :M])
(defn to-kw "Convert a string to symbols" [s] (->> s (map str/upper-case) (map keyword)))
(defn valid-roman "Predicate that checks if a value is a roman numeral" [d]
  (some #{d} *valid-roman-digits*))
(defn str->roman "Convert string to valid keyword vector" [s] (filter valid-roman (to-kw s)))
(defn roman->str [r] (str/join (map name r)))

(defn roman->unpacked [r]
  (case (roman->str r)
    "IV" (str->roman "IIII")
    "IX" (str->roman "VIIII")
    "XL" (str->roman "XXXX")
    "XC" (str->roman "LXXXX")
    "CD" (str->roman "CCCC")
    "CM" (str->roman "DCCCC")
    r))
(defn unpack "Expand by replacing subtracting forms" [numeral]
  (mapcat roman->unpacked (partition-all 2 numeral)))
(defn join "Join to roman numbers, return the sorted version" [n1 n2]
  (let [key-fn #(.indexOf (reverse *valid-roman-digits*) %)]
    (sort-by key-fn (concat n1 n2))))
(defn pack "Take an expanded numeral and pack it using subtracting forms" [numeral] numeral)

(defn add-roman [a b]
  (let [a-norm (unpack (str->roman a))
        b-norm (unpack (str->roman b))
        merged (join a-norm b-norm)]
    (roman->str (pack merged))))

;; TODO: The pack step.. perhaps it's easier to group the numeral {:X 3 :M 1} and use that to adjust according to subtracting forms?
