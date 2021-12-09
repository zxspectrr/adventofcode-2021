(ns advent-2021.day7
  (:require [clojure.string :as str]))

(defn parse-long [input-string]
  (-> input-string Double/parseDouble long))

(defn load-values []
  (->> (slurp "resources/day7-small.txt")
       (#(str/split % #","))
       (map parse-long)))

(defn fuel-cost [position destination]
  (->> (- position destination)
       (Math/abs)))

(defn find-costs [crabs destination]
  (->> (map #(fuel-cost % destination) crabs)
       (reduce +)))

(defn index-of-smallest [col]
  (first (apply min-key second (map-indexed vector col))))

(defn find-cheapest-cost [crabs]
  (as-> crabs _
        (apply max _)
        (range (inc _))
        (map #(find-costs crabs %) _)
        (index-of-smallest _)))

(comment
  (find-cheapest-cost input))

(def input (load-values))
