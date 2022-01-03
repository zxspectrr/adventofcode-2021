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
         [x y z])))

(defn do-step [cuboid [on-off step-coovals]]
  (let [step-coords (step step-coovals)]
    (into cuboid step-coords)))
    ;(if (= 1 on-off)
    ;  (into cuboid step-coords)
    ;  (set/difference cuboid step-coords))))

(defn process [steps]
  (reduce do-step #{} steps))

(defn part1 []
  (->> (get-input lines)
       process
       count))

(defn clean-value [v]
  (cond (<= v -50) -50
        (>= v 50) 50
        :else v))

(defn squash-coords-fn [reduce-fn group-fn coords]
  (->> (map (fn [a] (map group-fn a)) coords)
       (u/pivot)
       (map #(reduce reduce-fn %))))

(defn min-xyz [coords] (squash-coords-fn min first coords))
(defn max-xyz [coords] (squash-coords-fn max second coords))

(defn squash-min-max [coords]
  (mapv vector (min-xyz coords) (max-xyz coords)))

(defn clean-coord-ranges [ranges]
  (map (fn [[a b]] (tap> a) [(clean-value a) (clean-value b)]) ranges))

(defn squash-coord-ranges [coords]
  (let [on-off (ffirst coords)]
    [on-off (->> (mapv second coords)
                 squash-min-max
                 clean-coord-ranges)]))


(comment

  (squash-coord-ranges coords)

  (map squash-coord-ranges (partition-by first (get-input lines)))

  (def flat-coords [[[2253 11637] [73284 83993] [12224 22275]]
                    [[18360 27676] [11300 36836] [-92072 -72331]]
                    [[37724 70027] [-59694 -27177] [-56310 -28993]]])



  (def coords [[0 [[2253 11637] [73284 83993] [12224 22275]]]
               [0 [[18360 27676] [11300 36836] [-92072 -72331]]]
               [0 [[37724 70027] [-59694 -27177] [-56310 -28993]]]])

  (step [[59948 77225] [-23808 -9387] [7715 32589]])

  (reduce (fn [c [on-off step-vals]]
            (tap> [on-off step-vals])
            c)
          #{}
          (get-input lines))

  (process [[1 [[59948 77225] [-23808 -9387] [7715 32589]]]])

  (get-input [[1 [[59948 77225] [-23808 -9387] [7715 32589]]]])



  ,)
