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

(defn find-winning-boards [boards]
  (filter winning-board? boards))

(defn update-boards [boards score]
  (map #(mark-board % score) boards))

(defn run-games [boards scores]
  (loop [bs boards
         sc scores
         previous-scores []]
    (let [new-score (first sc)
          new-boards (update-boards bs new-score)
          winning-boards (find-winning-boards new-boards)]
      (if (or (= 1 (count sc)) (not (empty? winning-boards)))
        [winning-boards new-score (conj previous-scores new-score)]
        (recur new-boards (rest sc) (conj previous-scores new-score))))))

(defn score-for-board [board winning-score]
  (->> board
       (mapcat #(do %))
       (filter #(false? (second %)))
       (map first)
       (reduce +)
       (* winning-score)))

(defn part1 []
  (let [[scores boards _] (load-scores-and-boards)
        result (run-games boards scores)
        [winning-boards winning-score] result]
    (score-for-board (first winning-boards) winning-score)))

(comment

  (loop [games boards
         sc scores]
    (if (= 1 (count games))
      (do (println games) (score-for-board (first games) sc))
      (let [[winning-boards winning-score previous-scores] (run-games boards scores)
            new-boards (filter #(not= (first winning-boards) %) boards)
            rest-of-scores (drop (count previous-scores) sc)]
        (print new-boards)
        (println winning-score)
        (println rest-of-scores)
        (println previous-scores)
        nil)))
        ;(recur new-boards rest-of-scores)))
  ;(recur new-boards rest-of-scores))))


  (drop)

  (def winning-boardz (first (run-games boards scores)))
  (first winning-boardz)
  (filter #(not= (first winning-boardz) %) boards)

winning-board

  (def list1 [1 2 3 4 5 6])
  (def list2 [1 2 3 4])

  (drop (count list2) list1)

  (filter #(not (is-present %) = [2 3] %) [[5 6] [1 2] [2 3]])

  (def scores (first (load-scores-and-boards)))
  (def boards (second (load-scores-and-boards)))
  (part1)






