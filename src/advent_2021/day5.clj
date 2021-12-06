(ns advent-2021.day5
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]))

(defn parse-coords [coord-string]
  (->> (s/split coord-string #",")
       (map (fn [string-num] (Integer/parseInt string-num)))
       ((fn [[x y]] {:x x :y y}))))

(comment
  (load-lines)
  (straight-lines))

(defn find-line-type [line]
  (let [{start :start end :end} line]
    (if (= (:x start) (:x end)) :horizontal
      (if (= (:y start) (:y end)) :vertical))))

(defn string-to-line [line-string]
  (->> (s/split line-string #" -> ")
       (map parse-coords)
       (sort-by (juxt :x :y))
       ((fn [coords]
          {:start (first coords)
           :end (second coords)}))
       (#(assoc % :type (find-line-type %)))))

(defn load-lines []
  (->> (slurp "resources/small-lines.txt")
       (s/split-lines)
       (map #(string-to-line %))))

(defn straight-line? [{type :type}] (some? type))

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


(comment
  (part1)
  (lines-for-point {:x 7 :y 4} (straight-lines)))
