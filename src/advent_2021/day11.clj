(ns advent-2021.day11
  (:require [clojure.string :as str]
            [advent-2021.utils :as u]))

(def lines
  (->> (slurp "resources/day11.txt")
       (str/split-lines)
       (mapv (fn [line] (->> (re-seq #"\d" line)
                             (mapv u/parse-int))))))

(defn build-grid []
  (->> (for [x (range 0 (count (first lines)))
             y (range 0 (count lines))]
         [[x y] (get-in lines [y x])])
       (into {})))

(defn find-point [point grid]
  (get grid point))

(defn get-neighbours [[x y] grid]
  (let [points [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]]
    (filter #(some? (find-point % grid)) points)))

(defn flash-value? [v] (> v 9))

(defn flash? [point grid]
  (flash-value? (find-point point grid)))

(defn increment-points [points grid]
  (reduce (fn [g p] (update g p inc))
          grid points))

(defn increment-all-points [grid]
  (increment-points (keys grid) grid))

(defn update-for-flashing-point [grid point]
  (-> (get-neighbours point grid)
      (increment-points grid)))

(defn update-for-flashing-points [flash-points grid]
  (reduce update-for-flashing-point grid flash-points))

(defn process-flashed [grid]
  (loop [grid grid
         flashed #{}]
    (let [all-points (keys grid)
          can-flash (remove flashed all-points)
          flashing (filter #(flash? % grid) can-flash)
          new-grid (update-for-flashing-points flashing grid)]
      (if (empty? flashing)
        new-grid
        (recur new-grid (into flashed flashing))))))

(defn kill-flashed [grid]
  (u/map-vals (fn [v] (if (flash-value? v) 0 v)) grid))

(defn process-grid [grid]
  (->> (increment-all-points grid)
       (process-flashed)
       (kill-flashed)))

(defn part1 []
  (->> (iterate process-grid (build-grid))
       (take 101)
       (rest)
       (mapcat #(filter zero? (vals %)))
       (count)))

(defn part2 []
  (time
    (->> (iterate process-grid (build-grid))
         (take-while #(not (every? zero? (vals %))))
         (count))))