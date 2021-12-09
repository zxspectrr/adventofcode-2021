(ns advent-2021.day7
  (:require [clojure.string :as str]))

(defn parse-long [input-string]
  (-> input-string Double/parseDouble long))

(defn load-values []
  (->> (slurp "resources/day7.txt")
       (#(str/split % #","))
       (map parse-long)))

(defn fuel-cost [position destination]
  (->> (- position destination)
       (Math/abs)))

(defn find-costs [crabs destination]
  (->> (map #(fuel-cost % destination) crabs)
       (reduce +)))

(defn find-range [crabs]
  (range (inc (apply max crabs))))

(defn min-cost [crabs]
  (->> (map #(find-costs crabs %) (find-range crabs))
       (apply min)))

(defn part1 []
  (min-cost (load-values)))