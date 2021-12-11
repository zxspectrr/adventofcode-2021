(ns advent-2021.day9
  (:require [clojure.string :as str]))

(defn parse-int [str]
  (Integer/parseInt str))

(def input
  (->> (slurp "resources/day9-small.txt")
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

(def line (second input))

(defn do-find-basin-neighbours [basin-point points]
  (->> (find-neighbours basin-point points)
       (filter (fn [p] (< (:height p) 9)))))

(def cached-find-basin-neighbours (memoize do-find-basin-neighbours))

(defn do-find-basin-neighbours [basin-points points]
  (let [new-points (->> (mapcat #(cached-find-basin-neighbours % points) basin-points)
                        (set)
                        (into basin-points))]
    (if (= (count new-points) (count basin-points))
      new-points
      (recur new-points points))))

(def basin-points (first (build-points input)))

(def points (build-points input))

(first points)

(comment
  (count (do-find-basin-neighbours #{{:height 8, :coords [1 2]}} points))

  (defn find-basin-neighbours [point basin-members points]
    ;(loop [point point]
      (let [neighbours (do-find-basin-neighbours point points)]
        (if (empty? neighbours)
          basin-members
          (->> (map #(find-basin-neighbours % basin-members points) neighbours))
          (recur)))))

