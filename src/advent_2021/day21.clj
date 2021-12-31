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

(def game-state {:roll-count      0
                 :previous-roll   0
                 :dice-count      3
                 :dice-size       100
                 :winning-score   1000
                 :board-positions starting-positions
                 :scores          {1 0
                                   2 0}
                 :turn            1})

(defn winner? [{:keys [winning-score scores]}]
  (or (>= (get scores 1) winning-score)
      (>= (get scores 2) winning-score)))

(def continue? (complement winner?))

(defn update-board [{:keys [board-positions turn dice-count] :as game-state} rolls]
  (let [board-pos (get board-positions turn)
        total-roll (apply + rolls)
        new-pos (move board-pos total-roll)]

    (-> (assoc game-state :previous-roll (last rolls))
        (update :roll-count (partial + dice-count))
        (assoc :turn (get-next-turn turn))
        (update-in [:scores turn] (partial + new-pos))
        (assoc-in [:board-positions turn] new-pos))))

(defn step [{:keys [previous-roll dice-count dice-size] :as game-state}]
  (let [rolls (combined-roll previous-roll dice-size dice-count)]
    (update-board game-state rolls)))

(defn run-game [game-state]
  (take-while continue? (iterate step game-state)))

(defn part-1 []
  (let [final-roll (->> game-state run-game last step)
        {:keys [scores roll-count]} final-roll
        losing-score (->> (vals scores) sort first)]
    (* losing-score roll-count)))

(defn next-step [previous-states roll]
  (let [recent-game-state (last previous-states)]
    (conj previous-states (update-board recent-game-state [roll]))))

(defn historic-step [previous-states roll]
  (next-step previous-states roll))

(def previous-states [{:roll-count 0,
                       :previous-roll 0,
                       :dice-count 1,
                       :dice-size 3,
                       :winning-score 21,
                       :board-positions {1 1, 2 2},
                       :scores {1 0, 2 0},
                       :turn 1}
                      {:roll-count 1,
                       :previous-roll 1,
                       :dice-count 1,
                       :dice-size 3,
                       :winning-score 21,
                       :board-positions {1 2, 2 2},
                       :scores {1 2, 2 0},
                       :turn 2}])

(def games [[{:roll-count 0,
              :previous-roll 0,
              :dice-count 1,
              :dice-size 3,
              :winning-score 21,
              :board-positions {1 1, 2 2},
              :scores {1 0, 2 0},
              :turn 1}
             {:roll-count 1,
              :previous-roll 1,
              :dice-count 1,
              :dice-size 3,
              :winning-score 21,
              :board-positions {1 2, 2 2},
              :scores {1 2, 2 0},
              :turn 2}]])

(defn quantum-step [previous-states]
  (mapv (partial historic-step previous-states) [1 2]))

(defn quantum-games [games]
  (reduce (fn [games game]
            (let [games-for-game (quantum-step game)]
              (into games games-for-game)))
          []
          games))

(def initial-game-state
  (assoc game-state :dice-count 1 :dice-size 3 :winning-score 21))

(defn run-quantum [initial-game-state]
  (->> (iterate quantum-games [[initial-game-state]])
       (take 5)
       (last)))

(defn part-2 []
  (->> (assoc game-state :dice-count 1 :dice-size 3 :winning-score 21)
       run-quantum))


(comment
 ,)