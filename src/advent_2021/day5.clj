(ns advent-2021.day5
  (:require [clojure.string :as s]))

(defn parse-coords [coord-string]
  (->> (s/split coord-string #",")
       ((fn [[x y]] {:x x :y y}))))

(defn load-lines []
  (->> (slurp "resources/small-lines.txt")
       (s/split-lines)
       (map (fn [line] (s/split line #" -> ")))
       (map (fn [line]
              {:start (parse-coords (first line))
               :end (parse-coords (second line))}))))
(comment
  (load-lines)
  (s/split "0,9 -> 5,9" #" -> "))