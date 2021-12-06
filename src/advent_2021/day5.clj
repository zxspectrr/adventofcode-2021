(ns advent-2021.day5
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]))

(defn parse-coords [coord-string]
  (->> (s/split coord-string #",")
       (map (fn [string-num] (Integer/parseInt string-num)))))

(defn string-to-line [line-string]
  (->> (s/split line-string #" -> ")
       (map parse-coords)
       (sort-by first)
       (sort-by last)
       ((fn [[start end]]
          {:start {:x (first start)
                   :y (last start)}
           :end {:x (first end)
                 :y (last end)}}))))
(comment
  (load-lines))

(defn straight-line? [{start :start end :end}]
  (or (= (:x start) (:x end)) (= (:y start) (:y end))))

(defn load-lines []
  (->> (slurp "resources/lines.txt")
       (s/split-lines)
       (map #(string-to-line %))))

(defn straight-lines []
  (->> (load-lines)
       (filter straight-line?)))

(defn line-matches-point? [line point]
  (let [{start :start end :end} line
        horizontal-match?
        (and (<= (:x start) (:x point))
             (>= (:x end) (:x point))
             (= (:y start) (:y point)))
        vertical-match?
        (and (<= (:y start) (:y point))
             (>= (:y end) (:y point))
             (= (:x start) (:x point)))]
    (or horizontal-match? vertical-match?)))

(defn lines-for-point [point lines]
  (filter #(line-matches-point? % point) lines))

(defn create-grid []
  (map (fn [[x y]] {:x x :y y})
       (for [x (range 0 1000000)
             y (range 0 1000000)]
         [x y])))

(defn part1 []
  (let [lines (straight-lines)
        grid (create-grid)]
    (->> (filter #(>= (count (lines-for-point % lines)) 2) grid)
         (count))))

(comment
  (part1)
  (lines-for-point {:x 7 :y 4} (straight-lines))

  (line-matches-point? {:start {:x 7, :y 0}, :end {:x 7, :y 4}}
                       {:x 7 :y 4})
  ;
  ;(line-matches-point? {:start {:x 0, :y 9}, :end {:x 5, :y 9}}
  ;                     {:x 0 :y 9})


  (sort-by ) (straight-lines)

  (let [lines (straight-lines)
        grid (create-grid)]
    (->> (filter #(>= (count (lines-for-point % lines)) 2) grid)
         (count)))

  (->> (straight-lines)
       (lines-for-point {:x 3 :y 4}))
       ;(count))

   (load-lines)
  (s/split "0,9 -> 5,9" #" -> "))