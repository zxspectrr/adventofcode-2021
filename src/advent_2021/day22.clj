(ns advent-2021.day22
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(def lines (u/read-lines "resources/day22/input.txt"))

(defn parse-coord [line]
  (letfn [(get-coord-range [coord-str]
            (->> (str/split coord-str #"=")
                 second
                 (#(mapv u/parse-int (str/split % #"\.\.")))))]

    (let [on-off (->> (str/split line #" ")
                      first
                      (#(if (= "on" %) 1 0)))
          coords (->> (str/split line #" ")
                      second
                      (#(str/split % #","))
                      (mapv get-coord-range))]
      [on-off coords])))

(defn invalid-range? [[min max]]
  (or (> min 50) (< max -50)))

(defn valid-ranges [ranges]
  (empty? (filter invalid-range? ranges)))

(defn get-input [lines]
  (->> (mapv parse-coord lines)
       (filter (fn [[_ coords]]
                 (valid-ranges coords)))))

(defn step [[xs ys zs]]
  (letfn [(build-range [[min max]]
            (range min (inc max)))]

    (->> (for [x (build-range xs)
               y (build-range ys)
               z (build-range zs)]
           [x y z]))))

(defn do-step [cuboid [on-off step-coord-vals]]
  (let [step-coords (step step-coord-vals)]
    (if (= 1 on-off)
      (into cuboid step-coords)
      (set/difference cuboid step-coords))))

(defn process [steps]
  (reduce do-step #{} steps))

(defn part1 []
  (->> (get-input lines)
       process
       count))