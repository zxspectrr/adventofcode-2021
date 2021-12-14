(ns advent-2021.day11
  (:require [advent-2021.utils :as u]))

(def lines (->> (u/read-all-lines "resources/day11.txt")
                (mapv #(->> (re-seq #"\d" %)
                            (mapv u/parse-int)))))

(defn build-grid []
  (->> (for [x (range (count (first lines)))
             y (range (count (first lines)))]
         [[x y] (get-in lines [y x])])
       (into {})))

(defn display-grid [grid]
  (reduce (fn [l [[x y] v]] (assoc-in l [y x] v))
          lines
          grid))

(defn find-energy [grid point]
  (get grid point))

(defn find-neighbours [grid [x y]]
  (let [points [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]]
    (filter #(find-energy grid %) points)))

(defn flash-threshhold? [v] (> v 9))

(defn flashing? [grid point]
  (flash-threshhold?
    (find-energy grid point)))

(defn increment-point [grid point]
  (update grid point inc))

(defn increment-all-points [grid]
  (reduce increment-point grid (keys grid)))

(defn find-flashing [grid points]
  (filter #(flashing? grid %) points))

(defn process-flash [grid point]
  (reduce increment-point grid
          (find-neighbours grid point)))

(defn process-flash-points [grid points]
  (reduce process-flash grid points))

(defn process-flashes [grid]
  (loop [grid grid
         flashed #{}]
    (let [points (keys grid)
          can-flash (remove flashed points)
          flashing (find-flashing grid can-flash)
          updated-grid (process-flash-points grid flashing)]
      (if (empty? flashing)
        updated-grid
        (recur updated-grid (into flashed flashing))))))

(defn kill-all-flashes [grid]
  (u/map-vals (fn [v] (if (flash-threshhold? v) 0 v)) grid))

(defn process [grid]
  (->> (increment-all-points grid)
       (process-flashes)
       (kill-all-flashes)))

(defn part1 []
  (->> (iterate process (build-grid))
       (take 101)
       (mapcat #(filter zero? (vals %)))
       (count)))

(defn part2 []
  (->> (iterate process (build-grid))
       (take-while #(not (every? zero? (vals %))))
       (count)))

