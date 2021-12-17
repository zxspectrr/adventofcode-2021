(ns advent-2021.day14
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]))

(def lines (u/read-lines "resources/day14/small.txt"))

(def template (apply vector (first lines)))

(def rule-map
  (->> (split-with #(not (= "" %)) lines)
       (second)
       (rest)
       (map #(str/split % #" -> "))
       (into {})))

(defn get-element [char-arr]
  (get rule-map (reduce str char-arr)))

(get-element (first (partition 2 1 "NNCB")))

