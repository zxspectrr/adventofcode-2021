(ns advent-2021.day15
  (:require [clojure.string :as str]
            [advent-2021.utils :as u]))

(def risk-values (->> (u/read-lines "resources/day15/small.txt")
                      (mapv (fn [line]
                              (mapv (fn [letter]
                                      (u/parse-int (str letter)))
                                    line)))))

(def grid (for [x (range (count (first risk-values)))
                y (range (count risk-values))]
            [x y]))

(defn get-risk [[x y]]
  (get-in risk-values [y x]))

(get-risk [9 7])
