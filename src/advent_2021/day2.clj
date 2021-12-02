(ns advent-2021.day2
  (:require [clojure.string :as s]))

(defn- get-coords [path]
  (let [parse-fn (fn [line]
                   (let [[first second] (s/split line #" ")]
                     [first (Integer/parseInt second)]))]
    (map parse-fn (s/split-lines (slurp path)))))

(defn get-total-for-direction [direction coords]
  (->> (filter #(= (first %) direction) coords)
       (map second)
       (reduce +)))

(defn part1 []
  (let [coords (get-coords "resources/steps.txt")
        horizontal (get-total-for-direction "forward" coords)
        up (* -1 (get-total-for-direction "up" coords))
        down (get-total-for-direction "down" coords)
        vertical (+ up down)]
    (* horizontal vertical)))

(defn part2 []
  (let [final-pos
        (->>
          coords
          (reduce (fn [vals item]
                    (let [previous (last vals)
                          last-aim (if previous (last previous) 0)
                          direction-value (second item)
                          aim (case (first item)
                                "up" (- last-aim direction-value)
                                "down" (+ last-aim direction-value)
                                last-aim)
                          result  (conj item aim)]
                      (conj vals result)))
                  [])
          (filter #(= "forward" (first %)))
          (map #(let [direction-value (second %)
                      aim (last %)
                      x direction-value
                      y (* aim direction-value)]
                  [x y]))
          (reduce (fn [vals item]
                    [(+ (first vals) (first item))
                     (+ (second vals) (second item))])
                  [0 0]))
        [x y] final-pos]
    (* x y)))


(comment)


