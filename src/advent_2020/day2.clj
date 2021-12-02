(ns advent-2020.day2
  (:require [clojure.string :as s]))

(defn- get-coords []
  (let [parse-fn (fn [line]
                   (let [[first second] (s/split line #" ")]
                     [first (Integer/parseInt second)]))]
    (map parse-fn (s/split-lines (slurp "resources/coords.txt")))))

(defn get-total-for-direction [direction coords]
  (->> (filter #(= (first %) direction) coords)
       (map second)
       (reduce +)))

(defn part1 []
  (let [coords (get-coords)
        horizontal (get-total-for-direction "forward" coords)
        up (* -1 (get-total-for-direction "up" coords))
        down (get-total-for-direction "down" coords)
        vertical (+ up down)]
    (* horizontal vertical)))

(comment
  (reduce + (map second (filter #(= (first %) "forward") (get-coords))))

  (let [coords (get-coords)
        horizontal (get-total-for-direction "forward" coords)
        up (* -1 (get-total-for-direction "up" coords))
        down (get-total-for-direction "down" coords)
        vertical (+ up down)]
    (* horizontal vertical)))


