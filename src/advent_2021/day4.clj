(ns advent-2021.day4
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]))

(defn expand-board [lines]
  {:board (map (fn [line]
                 (->> (s/split (s/trim line) #"\s+")
                      (map #(vector (Integer/parseInt %) false))))
           lines)
   :winner false})

(defn load-scores-and-boards []
  (let [lines (s/split-lines (slurp "resources/bingo.txt"))
        scores (->> (s/split (first lines) #",")
                    (map #(Integer/parseInt %)))
        boards (->> (filter #(not= "" %) (rest lines))
                    (partition 5)
                    (map expand-board))]
    [scores boards]))

(defn mark-board [board score]
  (map (fn [line]
        (map (fn [[board-value selected]]
                 [board-value
                  (or selected (= board-value score))]) line))
       board))

(defn pivot [dataset] (apply map vector dataset))

(defn check-row [board]
  (->> (map #(group-by (fn [item] (second item)) %) board)
       (filter (fn [{false-items false}] (nil? false-items)))
       (empty?)
       (not)))

(defn winning-board? [board]
  (or (check-row board) (check-row (pivot board))))

(defn update-board [board score]
  (->> (mark-board (:board board) score)
       ((fn [new-board] {:board new-board
                         :winner (winning-board? new-board)
                         :score (if (winning-board? new-board) score nil)}))))

(defn update-boards [boards score]
  (map #(update-board % score) boards))

(defn run-game [p-boards p-scores]
  (loop [boards p-boards
         scores p-scores
         winners []]
    (let [score (first scores)
          updated-boards (update-boards boards score)
          new-winners (filter #(:winner %) updated-boards)
          remaining-boards (filter #(not (:winner %)) updated-boards)
          all-winners (concat winners new-winners)]
      (if (= 1 (count scores))
        all-winners
        (recur remaining-boards (rest scores) all-winners)))))

(defn score-for-board [board]
  (->> (:board board)
       (mapcat #(do %))
       (filter #(false? (second %)))
       (map first)
       (reduce +)
       (* (:score board))))

(defn part1 []
  (->> (load-scores-and-boards)
       ((fn [[scores boards]] (run-game boards scores)))
       (first)
       (score-for-board)))

(defn part2 []
  (->> (load-scores-and-boards)
       ((fn [[scores boards]] (run-game boards scores)))
       (last)
       (score-for-board)))






