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

(defn mark-board [board score]
  (map (fn [line]
        (map (fn [[board-value selected]]
                 [board-value
                  (or selected (= board-value score))]) line))
       board))

(defn pivot [dataset] (apply map vector dataset))

(defn check-row [board]
  (->> (map #(group-by (fn [item] (second item)) %) board)
       (filter (fn [{falses false}] (nil? falses)))
       (empty?)
       (not)))

(defn winning-board? [board]
  (or (check-row board) (check-row (pivot board))))

(defn run-game [boards scores]
  (loop [bs boards
         sc scores]
    (let [new-boards (map #(mark-board % (first sc)) bs)]
      (if (= 1 (count sc))
        new-boards
        (recur new-boards (rest sc))))))

(comment
  (def boards (:boards (load-scores-and-boards)))
  (def scores (:scores (load-scores-and-boards)))

  boards

  (def board (first boards)))

  [board]

  (def horizontal-board (first (run-game [board] [22 13 17 11 0])))

  (def vertical-board (first (run-game [board] [22 8 21 6 1])))

  (winning-board? vertical-board)

  vertical-board
  horizontal-board
  (check-row horizontal-board)

  (pivot board)
  board

  (->> (map #(group-by (fn [item] (second item)) %) horizontal-board)
       (filter (fn [{falses false}] (nil? falses)))
       (empty?)
       (not))


  (defn check-board [board]
   )

  board

  (defn horizontal-board
    ())


  (map (fn [line]
         (map (fn [[score _]]
                [score (= score 7)])
              line))
       board)

  "test"




