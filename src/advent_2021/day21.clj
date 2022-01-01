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
                 :last-score      0
                 :turn            1})

(defn get-next-turn [player-number]
  (if (= 1 player-number) 2 1))

(defn move [position moves]
  (let [score (dec (+ position moves))]
    (nth (cycle (range 1 11)) score)))

(def cached-move (memoize move))

(defn incrementing-roll [previous-value]
  (if (>= previous-value 100) 1 (inc previous-value)))

(defn combined-roll [previous-value]
    (->> (iterate incrementing-roll previous-value)
         (take 4)
         (rest)
         (into [])))

(defn game-has-winner? [winning-score last-score]
  (>= last-score winning-score))

(defn continue? [{:keys [winning-score last-score]}]
  (not (game-has-winner? winning-score last-score)))

(defn update-board [{:keys [board-positions turn] :as game-state} total-roll]
  (let [board-pos (get board-positions turn)
        new-pos (cached-move board-pos total-roll)]

    (-> (assoc game-state :turn (get-next-turn turn))
        (assoc :roll total-roll)
        (update-in [:scores turn] (partial + new-pos))
        (assoc-in [:board-positions turn] new-pos))))

(def cached-update-board (memoize update-board))

(def get-total-roll
  (memoize (fn[rolls]
             (apply + rolls))))

(defn step [{:keys [previous-roles] :as game-state}]
  (let [new-rolls (combined-roll (last previous-roles))
        total-roll (get-total-roll new-rolls)]
    (-> (update-board game-state total-roll)
        (update :roll-count (partial + 3))
        (assoc :previous-roles new-rolls))))

(defn run-game [game-state]
  (drop-while continue? (iterate step game-state)))

(defn part-1 []
  (let [final-roll (->> game-state run-game first)
        {:keys [scores roll-count]} final-roll
        losing-score (->> (vals scores) sort first)]
    (* losing-score roll-count)))

(defn has-winner? [{:keys [winning-score last-score]}]
  (game-has-winner? winning-score last-score))

(def is-active-game? (complement has-winner?))

(defn has-active-games? [games]
  (seq (filter is-active-game? games)))

(def winning-score?
  (memoize (fn [old-position old-score roll]
             (>= (+ (move old-position roll) old-score) 21))))

(defn next-step [game win-count roll]
  (let [{:keys [turn scores board-positions]} game
        old-position (get board-positions turn)
        old-score (get scores turn)]
    (if (winning-score? old-position old-score roll)
      [game (update win-count turn inc)]
      (let [updated-board (update-board game roll)
            new-score (get (:scores updated-board) turn)]
        (if (>= new-score 21)
          [updated-board (update win-count turn inc)]
          [updated-board win-count])))))

(defn get-quantum-rolls []
  (let [rolls [1 2 3]]
    (for [a rolls b rolls c rolls]
      [a b c])))
(def totalled-quantum-rolls
  (mapv (partial apply +) (get-quantum-rolls)))
(comment
  (group-by #(reduce + %) (get-quantum-rolls))
  ,)

(defn quantum-step [game win-count]
  (mapv (partial next-step game win-count) totalled-quantum-rolls))

(defn quantum-games [[games win-count]]
  (reduce (fn [games game]
            (into games (quantum-step game win-count)))
          []
          games))

(defn run-quantum [initial-game-state win-count]
  (->> (iterate quantum-games [[initial-game-state] win-count])
       (take 3)
       last))
       ;(drop-while has-active-games?)
       ;first))

(defn part-2 []
  (-> (assoc game-state :winning-score 1)
      (run-quantum {1 0 2 0})))

       ;get-winners
       ;frequencies))

(comment

  (def initial-game-state (assoc game-state :winning-score 2))
  (def games [initial-game-state])
  (def game (first games))

  (def win-counts {1 0 2 0})

  (def initial-game-state (assoc game-state :dice-count 1 :dice-size 3 :winning-score 10))
  (def results (quantum-games (quantum-games [[initial-game-state]])))

  (* 27 27 27)

  ,)

