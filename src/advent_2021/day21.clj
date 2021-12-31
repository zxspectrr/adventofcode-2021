(ns advent-2021.day21)

(defn combined-roll [previous-value dice-size dice-count]
  (let [roller-fn (fn [previous-value]
                    (if (>= previous-value dice-size ) 1 (inc previous-value)))]

    (->> (iterate roller-fn previous-value)
         (take (inc dice-count))
         (rest))))

(defn move [position moves]
  (let [score (dec (+ position moves))]
    (nth (cycle (range 1 11)) score)))

(defn get-next-turn [player-number]
  (if (= 1 player-number) 2 1))

(def starting-positions {1 1 2 2})
;(def starting-positions {1 4 2 8})

(def game-state {:current-roll 0
                 :roll-count 0
                 :dice-count 3
                 :dice-size 100
                 :board-positions starting-positions
                 :scores {1 0
                          2 0}
                 :turn 1})

(defn continue? [max-score {:keys [scores]}]
  (and (< (get scores 1) max-score)
       (< (get scores 2) max-score)))

(defn step [{:keys [current-roll board-positions turn dice-count dice-size] :as game-state}]
  (let [board-pos (get board-positions turn)
        rolls (combined-roll current-roll dice-size dice-count)
        total-roll (apply + rolls)
        new-pos (move board-pos total-roll)]

    (-> (assoc game-state :current-roll (last rolls))
        (update :roll-count (partial + 3))
        (assoc :turn (get-next-turn turn))
        (update-in [:scores turn] (partial + new-pos))
        (assoc-in [:board-positions turn] new-pos))))

(defn run-game [max-score]
  (take-while (partial continue? max-score) (iterate step game-state)))

(defn part-1 []
  (let [final-roll (->> (run-game 1000) last step)
        {:keys [scores roll-count]} final-roll
        losing-score (->> (vals scores) sort first)]
    (* losing-score roll-count)))

(defn part-2 []
  (->> (run-game 21) last step))

(comment
 ,)