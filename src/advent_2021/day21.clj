(ns advent-2021.day21)

(defn do-roll [previous-value]
  (if (> previous-value 99 ) 1 (inc previous-value)))

(defn combined-roll [previous-value]
  (->> (iterate do-roll previous-value)
       (take (inc 3))
       (rest)))

(defn move [position moves]
  (let [score (dec (+ position moves))]
    (nth (cycle (range 1 11)) score)))

(defn get-next-turn [player-number]
  (if (= 1 player-number) 2 1))

(def starting-positions {1 1 2 2})
;(def starting-positions {1 4 2 8})

(def game-state {:current-roll 0
                 :roll-count 0
                 :board-positions starting-positions
                 :scores {1 0
                          2 0}
                 :turn 1})

(defn winner? [{:keys [scores]}]
  (or (>= (get scores 1) 1000)
      (>= (get scores 2) 1000)))

(def continue? (complement winner?))

(defn step [{:keys [current-roll board-positions turn] :as game-state}]
  (let [board-pos (get board-positions turn)
        rolls (combined-roll current-roll)
        total-roll (apply + rolls)
        new-pos (move board-pos total-roll)]

    (-> (assoc game-state :current-roll (last rolls))
        (update :roll-count (partial + 3))
        (assoc :turn (get-next-turn turn))
        (update-in [:scores turn] (partial + new-pos))
        (assoc-in [:board-positions turn] new-pos))))

(defn run-game []
  (take-while continue? (iterate step game-state)))

(defn part-1 []
  (let [final-roll (->> (run-game) last step)
        {:keys [scores roll-count]} final-roll
        losing-score (->> (vals scores) sort first)]
    (* losing-score roll-count)))

(comment
 ,)