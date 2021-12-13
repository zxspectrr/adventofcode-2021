(ns advent-2021.day11
  (:require [clojure.string :as str]))

(defn parse-int [str] (Integer/parseInt str))

(def lines
 (->> (slurp "resources/day11-smaller.txt")
      (str/split-lines)
      (mapv (fn [line] (->> (re-seq #"\d" line)
                            (mapv parse-int))))))

(defn build-grid []
  (for [x (range 0 (count (first lines)))
        y (range 0 (count lines))]
    {:x x :y y :e (get-in lines [y x])}))

(def grid (build-grid))

(defn find-point [[x y] grid]
  (first (filter (fn [{gridx :x gridy :y}]
                   (and (= x gridx) (= y gridy)))
                 grid)))

(defn get-neighbours [{:keys [x y]} grid]
  (let [points [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]]
    (keep #(find-point % grid) points)))

(get-neighbours (find-point [0 0] grid) grid)

(defn flash? [{:keys [e]}] (> e 9))

(defn increment-all [points]
  (map (fn [{:keys [e] :as point}]
         (assoc point :e (inc e))) points))

(defn find-flashed [grid]
  (filter flash? grid))

(defn increment-neighbours [point grid]
  (->> (get-neighbours point grid)
       (increment-all)))

(defn update-grid-for-flash-point [point grid]
  (->> (get-neighbours point grid)
       (increment-all)))


(defn process [points grid]
  (loop [points points
         grid grid]
    (let [flashed (filter flash? points)
          neighbours (map #(increment-neighbours % grid) flashed)])))


(find-flashed (increment-all grid))

(->> (increment-all grid)
     (filter flash?))

(def next-line (increment-all grid))
