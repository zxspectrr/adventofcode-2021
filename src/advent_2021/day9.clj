(ns advent-2021.day9
  (:require [clojure.string :as str]))

(defn parse-int [str]
  (Integer/parseInt str))

(def input
  (->> (slurp "resources/day9.txt")
       (str/split-lines)
       (map #(->> (into [] (map parse-int (re-seq #"\d" %)))))
       (into [])))

(defn build-neighbour-map [input]
  (map-indexed
    (fn [idx line]
      (map-indexed
        (fn [idx-inner point]
          (let [upper (get-in input [(dec idx) idx-inner])
                lower (get-in input [(inc idx) idx-inner])
                left (get line (dec idx-inner))
                right (get line (inc idx-inner))]
            {:point point :neighbours (remove nil? [upper lower left right])}))
        line))
    input))

(defn smaller-than-neighbours? [point neighbours]
  (< point (reduce min neighbours)))

(defn find-smallest-points [grid-line]
  (->> (filter (fn [{:keys [point neighbours]}]
                 (smaller-than-neighbours? point neighbours))
               grid-line)
       (map (fn [{:keys [:point]}] point))))

(defn part1 []
  (->>
    (build-neighbour-map input)
    (mapcat find-smallest-points)
    (map inc)
    (reduce +)))

(comment

  (defn second-to-last [coll]
    (nth coll (dec (dec (count coll)))))

  (defn low-points-for-line [line]
    (letfn [(second-to-last [col])])

    (->>
      (second-to-last line)
      (conj line)
      (partition 2 1)
      (filter (fn [[a b]] (< a b)))
      (map (fn [[a _]] a))))

  (comment
    (->> (map low-points-for-line input)
         (map inc)
         (reduce +))

    (->>
      (second-to-last test-line)
      (conj test-line)
      (partition 2 1)
      (filter (fn [[a b]] (< a b)))
      (map (fn [[a _]] (inc a))))
    (count)))