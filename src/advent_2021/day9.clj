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

(defn find-low-points [points]
  (filter #(smaller-than-neighbours? % points) points))

(defn part1 []
  (->> (build-points input)
       (find-low-points)
       (map #(inc (:height %)))
       (reduce +)))

(defn is-top-height? [point]
  (= (:height point) 9))

(def do-find-basin-neighbours-neighbours
  (memoize
    (fn [basin-point points]
      (->> (find-neighbours basin-point points)
           (remove is-top-height?)))))

(defn build-basin [basin-points points]
  (let [new-points (->> (mapcat #(do-find-basin-neighbours-neighbours % points)
                                basin-points)
                        (set)
                        (into basin-points))]
    (if (= (count new-points) (count basin-points))
      new-points
      (recur new-points points))))

(defn part2 []
  (let [points (build-points input)
        basins (reduce (fn [basins point]
                         (conj basins (build-basin #{point} points)))
                       #{}
                       (find-low-points points))]
    (->> (map count basins)
         (sort)
         (reverse)
         (take 3)
         (reduce *))))