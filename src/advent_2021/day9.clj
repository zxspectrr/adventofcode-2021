(ns advent-2021.day9
  (:require [clojure.string :as str]))

(defn parse-int [str]
  (Integer/parseInt str))

(def input
  (->> (slurp "resources/day9-small.txt")
       (str/split-lines)
       (map #(->> (into [] (map parse-int (re-seq #"\d" %)))))))

(defn second-to-last [coll]
  (nth coll (dec (dec (count coll)))))

(defn low-points-for-line [line]
  (->>
    (second-to-last line)
    (conj line)
    (partition 2 1)
    (filter (fn [[a b]] (< a b)))
    (map (fn [[a _]] a))))

(def line (second input))

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
  (count))