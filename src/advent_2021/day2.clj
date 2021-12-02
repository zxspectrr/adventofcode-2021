(ns advent-2021.day2
  (:require [clojure.string :as s]))

(defn- get-steps [path]
  (let [parse-fn (fn [line]
                   (let [[direction distance] (s/split line #" ")]
                     [(keyword direction) (Integer/parseInt distance)]))]
    (map parse-fn (s/split-lines (slurp path)))))

(defn steps (get-steps "resources/steps.txt"))

(defn multiply-coords [[x y]]
  (* x y))

(defn part1 []
  (multiply-coords
    (reduce (fn [[x y] [direction distance]]
              (case direction
                :forward [(+ x distance) y]
                :up [x (- y distance)]
                :down [x (+ y distance)]))
            [0 0] steps)))

(defn part2 []
  (multiply-coords
    (reduce (fn [[x y aim] [direction distance]]
              (case direction
                :forward [(+ x distance) (+ y (* distance aim)) aim]
                :up [x y (- aim distance)]
                :down [x y (+ aim distance)]))
            [0 0 0] steps)))