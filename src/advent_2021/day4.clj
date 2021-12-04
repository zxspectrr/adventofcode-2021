(ns advent-2021.day4
  (:require [clojure.string :as s]))

(defn expand-board [lines]
  (map (fn [line]
         (->> (s/split (s/trim line) #"\s+")
              (map #(vector (Integer/parseInt %) false))))
       lines))

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

(defn find-winning-boards [boards]
  (filter winning-board? boards))

(defn run-games [boards scores]
  (loop [bs boards
         sc scores]
    (let [new-boards (map #(mark-board % (first sc)) bs)
          winning-boards (find-winning-boards new-boards)]
      (if (or (= 1 (count sc)) (not (empty? winning-boards)))
        [winning-boards (first sc)]
        (recur new-boards (rest sc))))))

(defn part1 []
  (let [[scores boards] (load-scores-and-boards)
        result (run-games boards scores)
        [winning-boards score] result
        sum-of-remaining
        (->> (first winning-boards)
             (mapcat #(do %))
             (filter #(false? (second %)))
             (map first)
             (reduce +))]
    (* sum-of-remaining score)))

(comment
  (part1))






