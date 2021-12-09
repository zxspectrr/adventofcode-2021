(ns advent-2021.day7
  (:require [clojure.string :as str]))

(defn parse-long [input-string]
  (-> input-string Double/parseDouble long))

(defn load-values []
  (->> (slurp "resources/day7-small.txt")
       (#(str/split % #","))
       (map parse-long)))

(defn simple-fuel-cost [position destination]
  (->> (- position destination)
       (Math/abs)))

(defn compound-fuel-cost [position destination]
  (->> (simple-fuel-cost position destination)
       (inc)
       (range 1)
       (reduce +)))

(defn find-costs [crabs destination cost-calculator]
  (->> (map #(cost-calculator % destination) crabs)
       (reduce +)))

(defn find-range [crabs]
  (range (inc (apply max crabs))))

(defn min-cost [crabs cost-calculator]
  (->> (map #(find-costs crabs % cost-calculator) (find-range crabs))
       (apply min)))

(defn part1 []
  (min-cost (load-values) simple-fuel-cost))

(defn part2 []
  (min-cost (load-values) compound-fuel-cost))