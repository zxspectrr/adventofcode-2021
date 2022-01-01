(ns advent-2021.day21-new)

(def starting-pos {1 4 2 8})
(def next-turn (partial {1 2 2 1}))

(next-turn 2)

(defn move [position roll]
  (let [score (dec (+ position roll))]
    (nth (cycle (range 1 11)) score)))

(def cached-move (memoize move))

(def initial-state {:board-positions starting-pos
                    :scores          {1 0 2 0}
                    :turn            1})

(defn step [{:keys [turn board-positions] :as state} roll]
  (let [pos (get board-positions turn)
        new-pos (cached-move pos roll)]
    (-> (update state :turn next-turn)
        (assoc-in [:board-positions turn] new-pos)
        (update-in [:scores turn] (partial + new-pos)))))

(defn score [rolls]
  (reduce step initial-state rolls))

(defn find-winner [rolls winning-score]
  (let [{:keys [scores turn]} (score rolls)
        previous-player (next-turn turn)
        previous-score (get scores previous-player)]
    (when (>= previous-score winning-score)
      previous-player)))

(def cached-find-winner find-winner)

(comment
  (cached-find-winner [1 1 1 1 1 1 1 1])

  (first (drop 100 (iterate cached-find-winner [1 1 1 1 1 1 1 1])))
  (time (first (drop 444356092776315 (repeat (cached-find-winner [1 1 1 1 1 1 1])))))

  (step initial-state 1),)


