(ns advent-2021.day2
  (:require [clojure.string :as s]))

(defn- get-steps [path]
  (let [parse-fn (fn [line]
                   (let [[direction distance] (s/split line #" ")]
                     [(keyword direction) (Integer/parseInt distance)]))]
    (map parse-fn (s/split-lines (slurp path)))))

(defn get-total-for-direction [direction coords]
  (->> (filter #(= (first %) direction) coords)
       (map second)
       (reduce +)))

(defn answer [coords]
  (apply * coords))

(defn part1 []
  (answer
    (reduce (fn [[x y] [direction distance]]
              (case direction
                :forward [(+ x distance) y]
                :up [x (- y distance)]
                :down [x (+ y distance)]))
            [0 0] steps)))
;
;(defn part2 []
;  (let [final-pos
;        (->>
;          (get-steps "resources/steps.txt")
;          (reduce (fn [vals item]
;                    (let [_  previous (last vals)
;                          last-aim (if previous (last previous) 0)
;                          direction-value (second item)
;                          aim (case (first item)
;                                :up (- last-aim direction-value)
;                                :down (+ last-aim direction-value)
;                                last-aim)
;                          result  (conj item aim)]
;                      (conj vals result)))
;                  [])
;          (filter #(= :forward (first %)))
;          (map #(let [direction-value (second %)
;                      aim (last %)
;                      x direction-value
;                      y (* aim direction-value)]
;                  [x y]))
;          (reduce (fn [vals item]
;                    [(+ (first vals) (first item))
;                     (+ (second vals) (second item))])
;                  [0 0]))
;        [x y] final-pos]
;    (* x y)))

(comment


  (part1)
  (print small-steps)
  (reduce (fn [[x y] [direction distance]]
            (println distance)
            (println direction)
            (case direction
              :forward [(+ x distance) y]
              :up [x (- y distance)]
              :down [x (+ y distance)]))
          [0 0] small-steps)

  (print steps)
  (def steps (get-steps "resources/steps.txt"))
  (def small-steps (get-steps "resources/small-steps.txt")))


