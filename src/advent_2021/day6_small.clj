(ns advent-2021.day6-small
  (:require [clojure.string :as str]))

(defn parse-long [input-string]
  (-> input-string Double/parseDouble long))

(defn load-values []
  (->> (slurp "resources/day6.txt")
       (#(str/split % #","))
       (map parse-long)))

(def input (load-values))

(def initial (reduce
               (fn [vec [k v]]
                 (assoc vec k v))
               (vec (repeat 9 0))
               (frequencies input)))

(defn update-state [[zeroes & tail]]
  (-> (vec tail)
      (conj zeroes)
      (update 6 + zeroes)))

(def days (iterate update-state initial))

(defn part1 []
  (apply + (nth days 80)))

(defn part2 []
  (apply + (nth days 256)))

