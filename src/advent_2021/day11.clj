(ns advent-2021.day11
  (:require [clojure.string :as str]))

(def lines
  (->> (slurp "resources/day11.txt")
       (str/split-lines)
       (mapv (fn [line] (->> (re-seq #"\d" line)
                             (mapv #(Integer/parseInt %)))))))

(defn build-grid []
  (->> (for [x (range 0 (count (first lines)))
             y (range 0 (count lines))]
         [[x y] (get-in lines [y x])])
       (into (hash-map))))

(defn find-point [point grid]
  (get grid point))

(defn get-neighbours [[x y] grid]
  (let [points [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]]
    (filter #(some? (find-point % grid)) points)))

(defn flash? [point grid]
  (> (find-point point grid) 9))

(defn increment-points [points grid]
  (reduce (fn [g p] (update g p inc))
          grid points))

(defn increment-all [grid]
  (increment-points (keys grid) grid))

(defn update-for-flashing-points [flash-points grid]
  (reduce (fn [g p] (increment-points (get-neighbours p g) g))
          grid
          flash-points))

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

(defn map-vals [f m] (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn kill-flashed [grid]
  (map-vals (fn [v] (if (> v 9) 0 v)) grid))

(defn process-grid [grid]
  (->> (increment-all grid)
       (process-flashed)
       (kill-flashed)))

(defn part1 []
  (->> (iterate process-grid (build-grid))
       (take 101)
       (rest)
       (mapcat #(filter zero? (vals %)))
       (count)))

(defn part2 []
  (->> (iterate process-grid (build-grid))
       (take-while #(not (every? zero? (vals %))))
       (count)))