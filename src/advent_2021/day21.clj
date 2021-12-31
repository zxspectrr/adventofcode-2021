(ns advent-2021.day21)

(def starting-positions {1 1 2 2})
(def starting-positions {1 4 2 8})

(def game-state {:roll-count      0
                 :roll            0
                 :previous-roles  [0]
                 :winning-score   1000
                 :board-positions starting-positions
                 :scores          {1 0
                                   2 0}
                 :turn            1
                 :has-winner      false})

(defn get-next-turn [player-number]
  (if (= 1 player-number) 2 1))

(defn move [position moves]
  (let [score (dec (+ position moves))]
    (nth (cycle (range 1 11)) score)))

(defn incrementing-roll [previous-value]
  (if (>= previous-value 100) 1 (inc previous-value)))

(defn combined-roll [previous-value]
    (->> (iterate incrementing-roll previous-value)
         (take 4)
         (rest)
         (into [])))

(defn game-has-winner? [{:keys [winning-score scores]}]
  (or (>= (get scores 1) winning-score)
      (>= (get scores 2) winning-score)))

(def continue? (complement game-has-winner?))

(defn update-winner-state [game-state]
  (assoc game-state :has-winner (game-has-winner? game-state)))

(defn update-board [{:keys [board-positions turn] :as game-state} total-roll]
  (let [board-pos (get board-positions turn)
        new-pos (move board-pos total-roll)]

    (-> (assoc game-state :roll total-roll)
        (update :roll-count (partial + 3))
        (assoc :turn (get-next-turn turn))
        (update-in [:scores turn] (partial + new-pos))
        (assoc-in [:board-positions turn] new-pos)
        (update-winner-state))))

(def cached-update-board (memoize update-board))

(def get-total-roll
  (memoize (fn[rolls]
             (apply + rolls))))

(defn step [{:keys [previous-roles] :as game-state}]
  (let [new-rolls (combined-roll (last previous-roles))]
    (-> (cached-update-board game-state (get-total-roll new-rolls))
        (assoc :previous-roles new-rolls))))

(defn run-game [game-state]
  (take-while continue? (iterate step game-state)))

(defn part-1 []
  (let [final-roll (->> game-state run-game last step)
        {:keys [scores roll-count]} final-roll
        losing-score (->> (vals scores) sort first)]
    (* losing-score roll-count)))

(defn has-winner? [game]
  (game-has-winner? (last game)))

(def is-active-game? (complement has-winner?))

(defn has-active-games? [games]
  (seq (filter is-active-game? games)))

(defn next-step [previous-states roll]
  (let [recent-game-state (last previous-states)]
    (if (not (:has-winner recent-game-state))
      (conj previous-states (cached-update-board recent-game-state roll))
      previous-states)))

(defn get-quantum-rolls []
  (let [rolls [1 2 3]]
    (for [a rolls b rolls c rolls]
      [a b c])))

(def totalled-quantum-rolls
  (mapv (partial apply +) (get-quantum-rolls)))

(comment
  (group-by #(reduce + %) (get-quantum-rolls))
  ,)

(defn quantum-step [game]
  (mapv (partial next-step game) totalled-quantum-rolls))

(defn quantum-games [games]
  (reduce (fn [games game]
            (let [games-for-game (quantum-step game)]
              (into games games-for-game)))
          []
          games))

(def initial-game-state (assoc game-state :winning-score 10))

(def games [[initial-game-state]])
(def game (first games))
(def quantum-rolls (get-quantum-rolls))

(defn get-winning-player [scores]
  (->> (reduce (fn [acc [k v]]
                 (if acc
                   (let [[ak av] acc]
                     (if (> v av) [k v] [ak av]))
                   [k v]))
               nil
               scores)
       first))

(defn get-winner [game]
  (->> (filter :has-winner game)
       first
       :scores
       get-winning-player))

(defn run-quantum [initial-game-state]
  (->> (iterate quantum-games [[initial-game-state]])
       (take-while has-active-games?)
       last
       quantum-games))

(defn part-2 []
  (->> (assoc game-state :winning-score 4)
       run-quantum
       count))
       ;get-winners
       ;frequencies))

(comment
  (def initial-game-state (assoc game-state :dice-count 1 :dice-size 3 :winning-score 10))
  (def results (quantum-games (quantum-games [[initial-game-state]])))

  (* 27 27 27)

  ,)


