(ns advent-2021.day9
  (:require [clojure.string :as str]))

(defn parse-int [str]
  (Integer/parseInt str))

(def input
  (->> (slurp "resources/day9.txt")
       (str/split-lines)
       (map #(->> (into [] (map parse-int (re-seq #"\d" %)))))
       (into [])))

(defn build-points [input]
  (->> (map-indexed
         (fn [y heights]
           (map-indexed
             (fn [x height]
               {:height height :coords [x y]})
             heights))
         input)
       (flatten)))

(defn find-neighbours [point points]
  (let [[x y] (:coords point)
        neighbour-coords #{[(dec x) y]
                           [(inc x) y]
                           [x (dec y)]
                           [x (inc y)]}]
    (filter #(some? (neighbour-coords (:coords %))) points)))

(defn smaller-than-neighbours? [{:keys [height] :as point} points]
    (->> (find-neighbours point points)
         (map :height)
         (reduce min)
         (< height)))

(defn part1 []
  (as-> (build-points input) _
        (filter #(smaller-than-neighbours? % _) _)
        (map #(inc (:height %)) _)
        (reduce + _)))
