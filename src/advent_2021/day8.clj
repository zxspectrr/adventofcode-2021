(ns advent-2021.day8
  (:require [clojure.string :as str]))


(defn sort-string [string]
  (reduce str (sort string)))

(defn parse-string [string]
  (->> (str/split string #" ")
       (map sort-string)))

(defn parse-line [line]
  (->>
    (str/split line #" \| ")
    ((fn [[signals digits]]
       [(parse-string signals)
        (parse-string digits)]))))

(def input
  (->> (slurp "resources/day8-small.txt")
       (str/split-lines)
       (map parse-line)))

(defn collection-contains? [coll value]
  (not (nil? (some #(= value %) coll))))

(defn unique-digit-length? [value]
  (collection-contains? [2 3 4 7] value))

(defn part1 []
  (->> (map second input)
       (flatten)
       (map count)
       (filter unique-digit-length?)
       (count)))

(def line "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
(def signals (first (first input)))

(def signals (first (parse-line line)))

(defn unique-signal [signal]
  (case (count signal)
    2 1
    4 4
    3 7
    7 8
    nil))

(unique-signal "abcc5")


(defn find-unique-signals [signals]
  (reduce (fn [acc x]
            (if-let [n (unique-signal x)]
              (assoc acc n x)
              acc))
          {}
          signals))


(def unique-signal-map (find-unique-signals signals))

(defn has-all-digits? [candidate reference]
  (clojure.set/subset? (set reference) (set candidate)))

(has-all-digits? "abc" "be")

(defn find-0-6-9 [signals unique-signal-map]
  (let [one-key (unique-signal-map 1)
        four-key (unique-signal-map 4)
        zero-six-nine (filter #(= 6 (count %)) signals)
        zero-key (first (filter #(not (has-all-digits? % one-key)) zero-six-nine))
        six-nine (filter #(not (= zero-key %)) zero-six-nine)
        nine-key (first (filter #(has-all-digits? % four-key) six-nine))
        six-key (first (filter #(not (= nine-key %)) six-nine))]
    (merge {0 zero-key
            9 nine-key
            6 six-key}
            ;9 nine-key 6 six-key}
           unique-signal-map)))

(find-0-6-9 signals unique-signal-map)




;
;if missing 1 chars it's 6
;if missing 4 chars its 0
;else 9
;
;if it has 1 its 3






