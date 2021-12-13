(ns advent-2021.day11
  (:require [clojure.string :as str]))

(defn parse-int [str] (Integer/parseInt str))

(def lines
  (->> (slurp "resources/day11-smaller.txt")
       (str/split-lines)
       (mapv (fn [line] (->> (re-seq #"\d" line)
                             (mapv parse-int))))))

(defn build-grid []
  (->> (for [x (range 0 (count (first lines)))
             y (range 0 (count lines))]
         [[x y] {:coord [x y] :e (get-in lines [y x])}])
       (into (hash-map))))

(def grid (build-grid))

(defn find-point [[x y] grid]
  (get grid [x y]))

(defn get-neighbours [[x y] grid]
  (let [points [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]]
    (keep #(find-point % grid) points)))

(defn flash? [{:keys [e]}] (> e 9))

(defn increment-all [points]
  (map (fn [{:keys [e] :as point}]
         (let [new-val (if (< e 10) (inc e) e)]
           (assoc point :e new-val)))
       points))

(defn update-grid [grid points]
  (reduce (fn [g point]
            (assoc g (:coord point) point))
          grid
          points))

(defn increment-points [grid points]
  (->> (increment-all points)
       (update-grid grid)))

(defn bump-grid [grid]
  (->> (vals grid)
       (increment-all)
       (update-grid grid)))

(defn update-for-flashing-points [flash-points grid]
  (reduce (fn [grid point]
            (let [neighbours (get-neighbours (:coord point) grid)]
              (increment-points grid neighbours)))
          grid
          flash-points))

(defn process [grid]

  (loop [grid (bump-grid grid)]
    (let [all-points (vals grid)
          flashed (filter flash? all-points)
          new-grid (update-for-flashing-points flashed grid)]
      (if (= (vals grid) (vals new-grid))
        new-grid
        (recur new-grid)))))

(comment
  (process grid))

(bump-grid grid)