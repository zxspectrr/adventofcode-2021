(ns advent-2021.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (->> (str/split line #" \| ")
       (map #(str/split % #" "))))

(def input
  (->> (slurp "resources/day8-small.txt")
       (str/split-lines)
       (map parse-line)))

(defn part1 []
  (->> (map second input)
       (flatten)
       (map count)
       (filter #{2 3 4 7})
       (count)))

(defn remove-subsets [s coll] (first (remove (partial set/subset? s) coll)))
(defn find-subset [s coll] (first (filter (partial set/subset? s) coll)))
(defn find-superset [s coll] (first (filter (partial set/superset? s) coll)))
(defn leftover [vals coll] (first (remove vals coll)))

(defn build-signal-map [test-readings]
    (let [frequency-map (group-by count (map set test-readings))
          one (first (frequency-map 2))
          four (first (frequency-map 4))
          seven (first (frequency-map 3))
          eight (first (frequency-map 7))
          zero-six-nine (frequency-map 6)
          six (remove-subsets one zero-six-nine)
          nine (find-subset four zero-six-nine)
          zero (leftover #{six nine} zero-six-nine)
          two-three-five (frequency-map 5)
          three (find-subset one two-three-five)
          five (find-superset six two-three-five)
          two (leftover #{three five} two-three-five)]

      {zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9}))

(defn extract-reading [signal-map readings]
  (->> (map #(signal-map (set %)) readings)
       (map str)
       (str/join)
       (Integer/parseInt)))

(defn score-for-readings [[signals readings]]
  (let [signal-map (build-signal-map signals)
        reading-vals (extract-reading signal-map readings)]
    [reading-vals]))

(defn part2 []
  (->> (mapcat score-for-readings input)
       (reduce +)))
