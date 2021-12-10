(ns advent-2021.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (->> (str/split line #" \| ")
       (map #(str/split % #" "))))

(def input
  (->> (slurp "resources/day8.txt")
       (str/split-lines)
       (map parse-line)))

(defn part1 []
  (->> (map second input)
       (flatten)
       (map count)
       (filter #{2 3 4 7})
       (count)))

(defn build-signal-map [test-readings]
  (letfn [(remove-subsets [s coll] (-> (remove (partial set/subset? s) coll) first))
          (find-subset [s coll] (-> (filter (partial set/subset? s) coll) first))
          (find-superset [s coll] (-> (filter (partial set/superset? s) coll) first))
          (leftover [vals coll] (-> (remove vals coll) first))]

    (let [frequency-map (group-by count (map set test-readings))
          one (-> (frequency-map 2) first)
          four (-> (frequency-map 4) first)
          seven (-> (frequency-map 3) first)
          eight (-> (frequency-map 7) first)
          zero-six-nine (frequency-map 6)
          six (remove-subsets one zero-six-nine)
          nine (find-subset four zero-six-nine)
          zero (leftover #{six nine} zero-six-nine)
          two-three-five (frequency-map 5)
          three (find-subset one two-three-five)
          five (find-superset six two-three-five)
          two (leftover #{three five} two-three-five)]

      {zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9})))

(defn extract-reading [readings signal-map]
  (->> (map (comp str signal-map set) readings)
       (str/join)
       (Integer/parseInt)))

(defn score-for-readings [[signals readings]]
  (->> (build-signal-map signals)
       (extract-reading readings)))

(defn part2 []
  (->> (map score-for-readings input)
       (reduce +)))