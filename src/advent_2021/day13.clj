(ns advent-2021.day13
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]))

(def instructions
  (->> (u/read-lines "resources/day13/small.txt")
       (split-with #(not (= "" %)))))

(defn parse-point [point-line]
  (->> (str/split point-line #",")
       (mapv u/parse-int)))

(defn parse-fold [fold-str]
  (-> (str/replace fold-str #"fold along " "")
      (str/split #"=")
      ((fn [[axis position]]
         [axis (u/parse-int position)]))))

(def folds
  (->> (rest (second instructions))
       (map parse-fold)))
(def points (map parse-point (first instructions)))
