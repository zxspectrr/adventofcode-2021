(ns advent-2021.day11
  (:require [clojure.string :as str]))

(defn parse-int [str] (Integer/parseInt str))

(def lines
  (->> (slurp "resources/day11-small.txt")
       (str/split-lines)
       (mapv (fn [line] (->> (re-seq #"\d" line)
                             (mapv parse-int))))))

(defn build-grid []
  (->> (for [x (range 0 (count (first lines)))
             y (range 0 (count lines))]
         [[x y] {:coord [x y] :e (get-in lines [y x])}])
       (into (hash-map))))

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
         (assoc point :e (inc e)))
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

(defn find-flashing [points flashed]
  (->> (filter flash? points)
       (remove #(flashed (:coord %)))))

(defn process-flashed [grid]
  (loop [grid grid
         flashed #{}]
    (let [all-points (vals grid)
          flashing (find-flashing all-points flashed)
          flashing-coords (map :coord flashing)
          new-grid (update-for-flashing-points flashing grid)]
      (if (empty? flashing)
        new-grid
        (recur new-grid (into flashed flashing-coords))))))

(defn kill-flashed [grid]
  (->> (vals grid)
       (map (fn [{:keys [coord e] :as point}]
              (if (> e 9)
                {:coord coord :e 0}
                point)))))

(defn process [grid]
  (->> (bump-grid grid)
       (process-flashed)
       (kill-flashed)
       (update-grid grid)))

(defn display-grid [grid]
  (mapv (fn [y]
          (mapv (fn [x]
                  (->> (find-point [x y] grid) :e))
                (range 0 (count (first lines)))))
        (range 0 (count lines))))

(defn process-times [times]

    (->> (take (inc times) (iterate process (build-grid)))
         (last)
         (display-grid)))

(defn part1 []
  (process-times 100)
  (->> (process (build-grid))
       (display-grid)))


(comment
  (def updated (process grid))

  (sort-by :coord (vals updated)))
