(ns advent-2021.day11
  (:require [clojure.string :as str]))

(defn parse-int [str] (Integer/parseInt str))

(def lines
 (->> (slurp "resources/day11-smaller.txt")
      (str/split-lines)
      (mapv (fn [line] (->> (re-seq #"\d" line)
                            (mapv parse-int))))))

;(defn build-grid []
;  (for [x (range 0 (count (first lines)))
;        y (range 0 (count lines))]
;    {:x x :y y :e (get-in lines [y x])}))

(defn build-grid []
  (->> (for [x (range 0 (count (first lines)))
             y (range 0 (count lines))]
         [[x y] {:coords [x y] :e (get-in lines [y x])}])
       (into (hash-map))))

(defn get-neighbours [[x y] grid]
  (let [points [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]]
    (keep grid points)))

(get-neighbours [0 0] grid)

(defn get-neighbour-energies [point]
  (->> (get-neighbours point)
       ;(map (fn [[x y]] {:coords [x y] :energy (get-energy [x y])}))
       (keep)
       (filter :energy)))

(defn flash? [energy] (> 9 energy))

;(defn point-to-item [[x y]]
;  {:coords [x y] :energy (get-energy [x y])})

(defn increment-all [input]
  (mapv (fn [line]
          (mapv inc line)) input))

(def next-line (increment-all lines))

(get-neighbour-energies [0 0])
