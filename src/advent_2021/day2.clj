(ns advent-2021.day2
  (:require [clojure.string :as s]))

(defn- parse [line]
  (let [[direction distance] (s/split line #" ")]
    [(keyword direction) (Integer/parseInt distance)]))

(defn- get-steps [path]
  (map parse (s/split-lines (slurp path))))

(def steps (get-steps "resources/steps.txt"))

(defn part1 []
  (->>
    steps
    (reduce (fn [[x y] [direction distance]]
              (case direction
                :forward [(+ x distance) y]
                :up [x (- y distance)]
                :down [x (+ y distance)]))
            [0 0])
    (apply *)))

(defn part2 []
  (->>
    steps
    (reduce (fn [[x y aim] [direction distance]]
              (case direction
                :forward [(+ x distance) (+ y (* distance aim)) aim]
                :up [x y (- aim distance)]
                :down [x y (+ aim distance)]))
            [0 0 0])
    (take 2)
    (apply *)))

(comment)
