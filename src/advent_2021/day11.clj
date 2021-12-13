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

(defn get-neighbours [point grid]
  (let [[x y] (:coord point)
        points [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]]
    (keep #(find-point % grid) points)))

(defn flash? [{:keys [e]}] (> e 9))

(defn increment-all [points]
  (map (fn [{:keys [e] :as point}]
         (assoc point :e (inc e))) points))

(defn find-flashed [grid]
  (filter flash? grid))

(defn increment-points [points grid]
  (->> (increment-all points)
       (reduce (fn [grid point]
                 (assoc grid (:coord point) (:e point)))
               grid)))

(defn increment-neighbours [point grid]
  (->> (get-neighbours point grid)
       (increment-all)
       (reduce (fn [grid point]
                 (assoc grid (:coord point) (:e point)))
               grid)))

(defn process-point [point grid]
  (if (flash? point)
    (increment-neighbours point grid)
    grid))

(defn process [grid flashpoints]
  (loop [flashed (filter flash? points)
         grid grid]

    (let [flashed (filter flash? points)
          neighbours (map #(increment-neighbours % grid) flashed)])))


(find-flashed (increment-all grid))

(->> (increment-all grid)
     (filter flash?))

(def next-line (increment-all grid))
