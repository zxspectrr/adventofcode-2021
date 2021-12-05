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
  (let [lines (s/split-lines (slurp "resources/small-bingo.txt"))
        scores (->> (s/split (first lines) #",")
                    (map #(Integer/parseInt %)))
        boards (->> (filter #(not= "" %) (rest lines))
                    (partition 5)
                    (map expand-board))]
    [scores boards]))

(defn mark-board [board score]
  (map (fn [line]
        (map (fn [[board-value selected]]
                 [board-value (or selected (= board-value score))]) line))
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

(defn process-score [game-state score]
  (let [{players :playing winners :winners} game-state
        new-boards (update-boards players score)
        {finished true playing false} (group-by #(true? (:winner %)) new-boards)]
    {:winners (concat winners finished)
     :playing playing}))

(defn process-scores [boards scores]
  (->> (reduce process-score
               {:winners [] :playing boards}
               scores)
       :winners))

(defn score-for-board [board]
  (->> (:board board)
       (mapcat #(do %))
       (filter #(false? (second %)))
       (map first)
       (reduce +)
       (* (:score board))))

(comment
  (part1))

(defn part1 []
  (->> (load-scores-and-boards)
       ((fn [[scores boards]] (process-scores boards scores)))
       (first)
       (score-for-board)))

(defn part2 []
  (->> (load-scores-and-boards)
       ((fn [[scores boards]] (process-scores boards scores)))
       (last)
       (score-for-board)))






