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

(def line (second input))

(defn is-top-height? [point]
  (= (:height point) 9))

(defn do-find-basin-neighbours-neighbours [basin-point points]
  (->> (find-neighbours basin-point points)
       (remove is-top-height?)))

(def cached-find-basin-neighbours (memoize do-find-basin-neighbours-neighbours))

(defn build-basin [basin-points points]
  (let [new-points (->> (mapcat #(cached-find-basin-neighbours % points) basin-points)
                        (set)
                        (into basin-points))]
    (if (= (count new-points) (count basin-points))
      new-points
      (recur new-points points))))

(def basin-points (first (build-points input)))

(def points (build-points input))

(first points)

(def filtered-points (remove is-top-height? points))

(comment
  (build-basin #{(first filtered-points)} (into #{} (first filtered-points)) points)

  (let [filtered-points (remove is-top-height? points)
        basins (reduce (fn [basins point]
                         (conj basins (build-basin #{point} points)))
                       #{}
                       filtered-points)]
    (->> (map count basins)
         (sort)
         (reverse)
         (take 3)
         (reduce *)))

    ;    basin-counts (reduce count basins)
    ;basin-counts)

  (count #{{:height 8, :coords [1 2]}
           {:height 8, :coords [5 2]}
           {:height 6, :coords [2 3]}
           {:height 7, :coords [3 3]}
           {:height 7, :coords [4 2]}
           {:height 5, :coords [2 2]}
           {:height 7, :coords [3 1]}
           {:height 8, :coords [1 4]}
           {:height 8, :coords [0 3]}
           {:height 6, :coords [3 2]}
           {:height 8, :coords [4 1]}
           {:height 7, :coords [1 3]}
           {:height 8, :coords [4 3]}
           {:height 8, :coords [2 1]}})

  (count (build-basin #{{:height 2, :coords [0 0]}} points))

  (defn find-basin-neighbours [point basin-members points]
    ;(loop [point point]
      (let [neighbours (build-basin point points)]
        (if (empty? neighbours)
          basin-members
          (->> (map #(find-basin-neighbours % basin-members points) neighbours))
          (recur)))))

