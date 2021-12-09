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

(defn unique-signal [signal]
  (case (count signal)
    2 1
    4 4
    3 7
    7 8
    nil))

(defn find-unique-signals [signals]
  (reduce (fn [acc x]
            (if-let [n (unique-signal x)]
              (assoc acc n x)
              acc))
          {}
          signals))

(defn has-all-digits? [candidate reference]
  (clojure.set/subset? (set reference) (set candidate)))

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
           unique-signal-map)))

(defn find-3-5-2 [signals signal-map]
  (let [one-key (signal-map 1)
        six-key (signal-map 6)
        three-five-two (filter #(= 5 (count %)) signals)
        three-key (first (filter #(has-all-digits? % one-key) three-five-two))
        five-two (filter #(not (= three-key %)) three-five-two)
        five-key (first (filter #(has-all-digits? % six-key) five-two))
        two-key (first (filter #(not (= five-key %)) five-two))]
    (merge {3 three-key
            5 five-key
            2 two-key}
           signal-map)))

(defn build-signal-map [signals]
  (->> (find-unique-signals signals)
       (find-0-6-9 signals)
       (find-3-5-2 signals)))
