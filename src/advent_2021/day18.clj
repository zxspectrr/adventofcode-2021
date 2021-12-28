(ns advent-2021.day18
  (:require [advent-2021.utils :as u]))

(def numbers
  (->> (u/read-lines "resources/day18/input.txt")
       (map read-string)))

(def numbers [[1,1] [2,2] [3,3] [4,4]])

(defn add-numbers [numbers]
  (reduce (fn [acc x]
            (if acc [acc x]
                    x)) nil numbers))

(comment

  ,)