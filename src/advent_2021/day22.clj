(ns advent-2021.day22
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(def lines (u/read-lines "resources/day22/smaller.txt"))

(defn parse-coord [line]
  (letfn [(get-coord-range [coord-str]
            (->> (str/split coord-str #"\.\.")
                 (mapv (comp u/parse-int (partial re-find #"\d+")))))]

    (let [on-off (->> (str/split line #" ")
                      first
                      (#(if (= "on" %) 1 0)))
          coords (->> (str/split line #" ")
                      second
                      (#(str/split % #","))
                      (mapv get-coord-range))]
      [on-off coords])))

(def input (mapv parse-coord lines))

(defn step [[value [xs ys zs]]]
  (letfn [(get-range [[min max]] (range min (inc max)))]
    (->> (for [x (get-range xs)
               y (get-range ys)
               z (get-range zs)]
           [[x y z] value])
         (into {}))))

(defn do-step [cuboid step-vals]
  (merge cuboid (step step-vals)))

(defn process [steps]
  (reduce do-step {} steps))

(comment
  (->> (process input)
       (filter (fn [[coord v]] (= v 1)))
       count))
