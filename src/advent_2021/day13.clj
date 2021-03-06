(ns advent-2021.day13
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(def instructions
  (->> (u/read-lines "resources/day13/input.txt")
       (split-with #(not (= "" %)))))

(defn parse-point [point-line]
  (->> (str/split point-line #",")
       (mapv u/parse-int)))

(defn parse-fold [fold-str]
  (-> (str/replace fold-str #"fold along " "")
      (str/split #"=")
      ((fn [[axis position]]
         [axis (u/parse-int position)]))))

(def folds
  (->> (rest (second instructions))
       (map parse-fold)))

(def points
  (->> (map parse-point (first instructions)) (set)))

(def fold-axis-map {"y" 1 "x" 0})

(defn beyond-fold? [point [direction position]]
  (let [axis (get fold-axis-map direction)
        point-position (get point axis)]
    (> point-position position)))

(defn fold-point [point [direction position]]
  (let [axis (get fold-axis-map direction)
        point-position (get point axis)
        distance (- point-position position)
        new-point-position (- point-position (* 2 distance))]
    (assoc point axis new-point-position)))

(defn apply-fold [points fold]
  (let [to-fold (set (filter #(beyond-fold? % fold) points))
        filtered-points (set/difference points to-fold)
        folded (map #(fold-point % fold) to-fold)]
     (into filtered-points folded)))

(defn max-val [axis points]
  (reduce max (map #(get % (get fold-axis-map axis)) points)))

(defn build-2d-vector [x y]
  (vec (repeat y (vec (repeat x ".")))))

(defn add-point-to-grid [grid [x y]]
  (assoc-in grid [y x] "#"))

(defn print-grid [grid]
  (doseq [line grid]
    (println line)))

(defn display-points [points]
  (let [max-x (inc (max-val "x" points))
        max-y (inc (max-val "y" points))
        grid (build-2d-vector max-x max-y)]
    (->> (reduce add-point-to-grid grid points)
         (print-grid))))

(defn part1 []
  (->> (apply-fold points (first folds))
       (count)))

(defn part2 []
  (->> (reduce apply-fold points folds)
       (display-points)))