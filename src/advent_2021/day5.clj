(ns advent-2021.day5
  (:require [clojure.string :as s]))

(defn parse-coords [coord-string]
  (->> (s/split coord-string #",")
       ((fn [[x y]] {:x x :y y}))))

(defn load-lines []
  (let [lines  (->> (slurp "resources/small-lines.txt")
                    (s/split-lines))
        cells (->> (map (fn [line] (s/split line #" -> ")) lines)
                   (map (fn [line]
                          {:start (parse-coords (first line))
                           :end (parse-coords (second line))})))]


    cells))

(comment
  (s/split "0,9 -> 5,9" #" -> "))