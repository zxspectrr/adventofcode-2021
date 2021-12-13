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
         [[x y] (get-in lines [y x])])
       (into (hash-map))))

(def grid (build-grid))

(defn find-point [point grid]
  (get grid point))

(defn get-neighbours [[x y] grid]
  (let [points [[(dec x) (inc y)] [x (inc y)] [(inc x) (inc y)]
                [(dec x) y] [(inc x) y]
                [(dec x) (dec y)] [x (dec y)] [(inc x) (dec y)]]]
    (filter #(some? (find-point % grid)) points)))

(defn flash? [point grid]
  (> (find-point point grid) 9))

(defn increment-points [points grid]
  (reduce (fn [g point]
            (update g point inc))
          grid
          points))

(defn bump-grid [grid]
  (increment-points (keys grid) grid))

(defn find-flashing [points flashed]
  (->> (filter #(flash? % grid) points)
       (remove flashed)))

(defn update-for-flashing-points [flash-points grid]
  (reduce (fn [g p]
            (increment-points (get-neighbours p g) g))
          grid
          flash-points))

(defn process-flashed [grid]
  (loop [grid grid
         flashed #{}]
    (let [all-points (keys grid)
          flashing (find-flashing all-points flashed)
          new-grid (update-for-flashing-points flashing grid)]
      (if (empty? flashing)
        new-grid
        (recur new-grid (into flashed flashing))))))

(defn kill-flashed [grid]
  (->> (vec grid)
       (filter (fn [[k v]] (> v 9)))
       (map (fn [[k v]] k))
       (reduce (fn [g point]
                 (assoc g point 0)) grid)))

(comment
  (process-grid grid))

(defn get-flash-count [grid]
  (->> (vals grid)
       (filter #(= 0 %))
       (count)))
(defn process-grid [grid]
  (->> (bump-grid grid)
       (process-flashed)))
       ;(kill-flashed)))

(defn process [{:keys [grid flashcount]}]
  (let [updated-grid (process-grid grid)
        updated-flashcount (+ flashcount (get-flash-count updated-grid))]
    {:grid updated-grid
     :flashcount updated-flashcount}))

(defn display-grid [grid]
  (mapv (fn [y]
          (mapv (fn [x]
                  (->> (find-point [x y] grid) :e))
                (range 0 (count (first lines)))))
        (range 0 (count lines))))

(defn init []
  {:grid (build-grid)
   :flashcount 0})

(defn process-times [times]
  (->> (take (inc times) (iterate process (init)))
       (last)
       (:flashcount)))

(defn part1 []
  (process-times 100))
