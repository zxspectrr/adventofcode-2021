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
         [x y])))

(def grid (build-grid))

(defn find-energy [[x y] lines]
  (get-in lines [y x]))

(defn get-neighbours [[x y]]
  [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
   [(dec x) y] [(inc x) y]
   [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]])

(defn get-neighbour-energies [[x y] lines]
  (keep #(find-energy % lines) (get-neighbours [x y])))

(get-neighbour-energies [0 0] lines)

(defn increment-point [[x y] lines]
  (->> (find-energy [x y] lines)
       (inc)
       (assoc-in lines [y x])))

(defn flash? [point lines]
  (> (find-energy point lines) 9))

(defn increment-points [points lines]
  (reduce (fn [lines point]
            (increment-point point lines))
          lines
          points))

(defn bump-all-points [lines]
  (increment-points grid lines))

(def lines (bump-all-points  lines))

(bump-all-neighbours)

(defn process [grid lines]
  (let [flashed (filter #(flash? % lines) grid)
        neighbours (mapcat)]

    (loop [grid grid
           flash-points (filter flash? grid)]
      (let [flashed (filter flash? points)
            neighbours (map #(increment-neighbours % grid) flashed)]))))

