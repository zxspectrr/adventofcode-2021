(ns advent-2021.day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (mapv (fn [line]
                   (mapv #(Integer/parseInt (str %)) line))
                 (str/split-lines (slurp "resources/day9.txt"))))

(defn adjacent-coordinates [x y]
  [[(dec x) y] [(inc x) y] [x (inc y)] [x (dec y)]])

(defn get-height [[x y]]
  (get-in input [y x]))

(defn adjacent-heights [[x y]]
  (keep get-height (adjacent-coordinates x y)))

(defn low-point [coord]
  (let [height (get-height coord)
        surrounding (adjacent-heights coord)]
    (< height (apply min surrounding))))

(defn build-all-coordinates [input]
  (let [num-cols (count (first input))
        num-rows (count input)]
    (for [x (range 0 num-cols)
          y (range 0 num-rows)]
      [x y])))

(defn low-points [all-coords]
  (let [coords all-coords]
    (filter low-point coords)))

(defn risk [coordinate]
  (inc (get-height coordinate)))

(defn part1 []
  (->> (build-all-coordinates input)
       (low-points)
       (map risk)
       (reduce +)))

(defn neighbours [[x y]]
  (->> (adjacent-coordinates x y)
       (filter #(let [height (get-height %)]
                  (and height (not= 9 height))))))

(defn basin [low-point]
  (loop [basin #{low-point}]
    (let [expanded (set/difference (set (mapcat neighbours basin)) basin)]
      (if (empty? expanded)
        basin
        (recur (into basin expanded))))))

(defn part2 []
  (->> (build-all-coordinates input)
       (low-points)
       (map basin)
       (sort-by count >)
       (take 3)
       (map count)
       (apply *)))