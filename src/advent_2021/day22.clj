(ns advent-2021.day22
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(def lines (u/read-lines "resources/day22/small.txt"))

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

(defn get-input [lines] (mapv parse-coord lines))

(defn sub-range [[min max]]
  (let [valid-range (range -50 51)]
    (->> (set (range min (inc max)))
         (set/intersection (set valid-range))
         vec
         sort)))

(defn step [[xs ys zs]]
  (->> (for [x (sub-range xs)
             y (sub-range ys)
             z (sub-range zs)]
         [x y z])
       (into #{})))

(defn do-step [cuboid [on-off step-coovals]]
  (let [step-coords (step step-coovals)]
    (if (= 1 on-off)
      (into cuboid step-coords)
      (set/difference cuboid step-coords))))

(defn process [steps]
  (reduce do-step #{} steps))

(defn part1 []
  (->> (get-input lines)
       process
       count))

(comment
  ,)
