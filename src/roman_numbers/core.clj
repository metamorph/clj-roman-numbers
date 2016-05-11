(ns roman-numbers.core
  "Functions for working with Roman numerals. The algorithms for addition etc are
  taken from here: http://turner.faculty.swau.edu/mathematics/materialslibrary/roman/"
  (:require [clojure.string :as str]))

(declare *valid-roman-digits*)

(defn to-kw "Convert a string to symbols"
  [s]
  (->> s (map str/upper-case) (map keyword)))

(defn valid-roman "Predicate that checks if a value is a roman numeral"
  [d]
  (some #{d} *valid-roman-digits*))

(defn str->roman "Convert string to valid keyword vector"
  [s] (filter valid-roman (to-kw s)))

(defn roman->str "Convert a roman numeral to string"
  [r] (str/join (map name r)))

(def *valid-roman-digits* "The only valid roman digits"
  [:I :V :X :L :C :D :M])

(def *grouping-table* "Grouping equivalence table"
  {(str->roman "IIIII") (str->roman "V")
   (str->roman "VV")    (str->roman "X")
   (str->roman "XXXXX") (str->roman "L")
   (str->roman "LL")    (str->roman "C")
   (str->roman "CCCCC") (str->roman "D")
   (str->roman "DD")    (str->roman "M")})

(def *subtractive-forms* "Subtractive forms"
  {(str->roman "IV") (str->roman "IIII")
   (str->roman "IX") (str->roman "XXXX")
   (str->roman "XL") (str->roman "VIIII")
   (str->roman "XC") (str->roman "LXXXX")
   (str->roman "CD") (str->roman "CCCC")
   (str->roman "CM") (str->roman "DCCCC")})

(defn starts-with? "Check if the head of +col+ is +s+"
  [col s] (let [sub (take (count s) col)] (= sub s)))

(defn map-and-replace
  "Traverse 'col' and pass build a new seq by prepending calling 'f'
  for each version of the collection. Can't explain it in another way. See unit-tests"
  [col f]
  (loop [result []
         col col]
      (if (empty? col)
        result
        (let [[replacement count] (f col)
              remainder (drop count col)]
          (recur (concat result replacement) remainder)))))

(defn roman-sort "Sort a roman numeral from large to small digits"
  [r]
  (sort-by #(.indexOf (reverse *valid-roman-digits*) %) r))

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
  (roman-sort (concat n1 n2)))
(defn pack "Take an expanded numeral and pack it using subtracting forms" [numeral] numeral)

(defn add-roman [a b]
  (let [a-norm (unpack (str->roman a))
        b-norm (unpack (str->roman b))
        merged (join a-norm b-norm)]
    (roman->str (pack merged))))

