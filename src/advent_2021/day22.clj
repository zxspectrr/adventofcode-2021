(ns advent-2021.day22
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(def lines (u/read-lines "resources/day22/smaller.txt"))

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

(defn valid-coords [[xs ys zs]]
  (letfn [(correct-size? [v] (and (>= v -50) (<= v 50)))
          (in-range? [[min max]] (and (correct-size? min) (correct-size? max)))]
    (and (in-range? xs) (in-range? ys) (in-range? zs))))

(defn get-input [lines]
  (->> (mapv parse-coord lines)))
       ;(filter (fn [[v coords]])))

(defn sub-range [[min max]]
  (let [valid-range (range -50 51)]
    (->> (set (range min (inc max)))
         (set/intersection (set valid-range))
         vec
         sort)))

(defn find-valid-sub-range [[xs ys zs]])

(defn step [[value [xs ys zs]]]
  (->> (for [x (sub-range xs)
             y (sub-range ys)
             z (sub-range zs)]
         [[x y z] value])
       (into {})))

(defn do-step [cuboid step-vals]
  (merge cuboid (step step-vals)))

(defn process [steps]
  (reduce do-step {} steps))

(defn find-on [coord-map]
  (filter (fn [[_ v]] (= v 1)) coord-map))

;(defn find-on [coord-map]
;  (let [r (sub-range [-50 50])
;        valid-coords (->> (for [x r y r z r]
;                            [x y z])
;                          set)]
;    (->> (filter (fn [[_ v]] (= v 1)) coord-map)
;         set
;         (set/intersection (valid-coords)))))

(defn part1 []
  (->> (get-input lines)
       process)
       find-on
       count)

(comment

  ,)
