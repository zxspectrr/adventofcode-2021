(ns advent-2021.day5
  (:require [clojure.string :as s]))

(defn parse-coords [coord-string]
  (->> (s/split coord-string #",")
       (map (fn [string-num] (Integer/parseInt string-num)))
       ((fn [[x y]] {:x x :y y}))))

(defn string-to-line [line-string]
  (->> (s/split line-string #" -> ")
       ((fn [coords]
          {:start (parse-coords (first coords))
           :end (parse-coords (second coords))}))))

(defn straight-line? [{start :start end :end}]
  (or (= (:x start) (:x end)) (= (:y start) (:y end))))

(defn load-lines []
  (->> (slurp "resources/small-lines.txt")
       (s/split-lines)
       (map #(string-to-line %))))

(defn straight-lines []
  (->> (load-lines)
       (filter straight-line?)))

(defn line-matches-point? [line point]
  (let [{start :start end :end} line
        horizontal-match?
        (and (<= (:x start) (:x point))
             (> (:x end) (:x point))
             (= (:y start) (:y point)))
        vertical-match?
        (and (<= (:y start) (:y point))
             (> (:y end) (:y point))
             (= (:x start) (:x point)))]
    (or horizontal-match? vertical-match?)))

(defn lines-for-point [point lines]
  (filter #(line-matches-point? % point) lines))

(comment


  (line-matches-point? {:start {:x 0, :y 9}
                        :end {:x 5, :y 9}}
                       {:x 3 :y 9})

  (line-matches-point? {:start {:x 0, :y 9}, :end {:x 5, :y 9}}
                       {:x 0 :y 9})

  (->> (straight-lines)
       (lines-for-point {:x 0 :y 9})
       (count))

  (load-lines)
  (s/split "0,9 -> 5,9" #" -> "))