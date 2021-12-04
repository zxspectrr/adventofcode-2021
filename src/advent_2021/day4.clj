(ns advent-2021.day4
  (:require [clojure.string :as s]))

(defn expand-board [lines]
  (map (fn [line]
         (->> (s/split (s/trim line) #"\s+")
              (map #(vector (Integer/parseInt %) false))))
       lines))

(defn load-scores-and-boards []
  (let [lines (s/split-lines (slurp "resources/small-bingo.txt"))
        scores (->> (s/split (first lines) #",")
                    (map #(Integer/parseInt %)))
        boards (->> (filter #(not= "" %) (rest lines))
                    (partition 5)
                    (map expand-board))]
    {:scores scores :boards boards}))

(comment
  (load-scores-and-boards))