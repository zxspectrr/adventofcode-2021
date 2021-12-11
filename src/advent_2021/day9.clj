(ns advent-2021.day9
  (:require [clojure.string :as str]))

(defn parse-int [str]
  (Integer/parseInt str))

(def input
  (->> (slurp "resources/day9.txt")
       (str/split-lines)
       (map #(->> (into [] (map parse-int (re-seq #"\d" %)))))
       (into [])))

(defn setup [input]
  (loop [previous nil
         xs input
         results []]
    (if (empty? xs)
      results
      (let [x (first xs)
            next (second xs)
            result {:x x :previous previous :next next}]
        (recur x (rest xs) (conj results result))))))

(defn smaller-than-neighbours? [value idx grid-line]
  (let [{:keys [x next previous]} grid-line
        left (get x (dec idx))
        right (get x (inc idx))
        bottom (get next idx)
        top (get previous idx)
        filtered (remove nil? [value left right bottom top])
        smallest (reduce min filtered)]
    (= value smallest)))

(defn find-smallest-points [grid-line]
  (->> (filter (fn [[idx itm]]
                 (smaller-than-neighbours? itm idx grid-line))
               (map-indexed (fn [idx itm] [idx itm]) (:x grid-line)))
       (map (fn [[_ itm]] itm))))

(defn part1 []
  (->>
    (setup input)
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