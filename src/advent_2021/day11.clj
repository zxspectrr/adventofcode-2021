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

;(defn build-grid []
;  (->> (for [x (range 0 (count (first lines)))
;             y (range 0 (count lines))]
;         [[x y] {:coords [x y] :e (get-in lines [y x])}])
;       (into (hash-map))))

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

(defn flash? [energy] (> 9 energy))

(defn increment-all [grid]
  (map (fn [{:keys [e] :as point}]
         (assoc point :e (inc e))) grid))

(def next-line (increment-all grid))
