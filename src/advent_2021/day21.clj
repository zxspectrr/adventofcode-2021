(ns advent-2021.day21
  (:require [clojure.set :as set]))

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
(def starting-positions {1 4 2 8})

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

(def game [{:roll-count 0,
            :previous-roll 0,
            :dice-count 1,
            :dice-size 3,
            :winning-score 10,
            :board-positions {1 1, 2 2},
            :scores {1 0, 2 0},
            :turn 1}
           {:roll-count 1,
            :previous-roll 1,
            :dice-count 1,
            :dice-size 3,
            :winning-score 10,
            :board-positions {1 2, 2 2},
            :scores {1 2, 2 0},
            :turn 2}
           {:roll-count 2,
            :previous-roll 1,
            :dice-count 1,
            :dice-size 3,
            :winning-score 10,
            :board-positions {1 2, 2 3},
            :scores {1 2, 2 3},
            :turn 1}
           {:roll-count 3,
            :previous-roll 1,
            :dice-count 1,
            :dice-size 3,
            :winning-score 10,
            :board-positions {1 3, 2 3},
            :scores {1 5, 2 3},
            :turn 2}
           {:roll-count 4,
            :previous-roll 1,
            :dice-count 1,
            :dice-size 3,
            :winning-score 10,
            :board-positions {1 3, 2 4},
            :scores {1 5, 2 7},
            :turn 1}
           {:roll-count 5,
            :previous-roll 2,
            :dice-count 1,
            :dice-size 3,
            :winning-score 10,
            :board-positions {1 5, 2 4},
            :scores {1 10, 2 7},
            :turn 2}
           {:roll-count 6,
            :previous-roll 1,
            :dice-count 1,
            :dice-size 3,
            :winning-score 10,
            :board-positions {1 5, 2 5},
            :scores {1 10, 2 12},
            :turn 1}])

(def games [game])

(defn has-winner? [game]
  (winner? (last game)))

(def is-active-game? (complement has-winner?))

(defn has-active-games? [games]
  (seq (filter is-active-game? games)))

(def quantum-rolls [1 2 3])

(defn quantum-step [previous-states]
  (mapv (partial historic-step previous-states) quantum-rolls))

(defn quantum-games [games]
  (reduce (fn [games game]
            (let [games-for-game (quantum-step game)]
              (into games games-for-game)))
          []
          games))

(def initial-game-state
  (assoc game-state :dice-count 1 :dice-size 3 :winning-score 2))

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
  (->> (filter winner? game)
       first
       :scores
       get-winning-player))

(defn get-winners [games]
  (map get-winner games))

(def quantum-rolls [1 2 3])

(defn rolls-for-games [games]
  (map #(map :previous-roll %) games))

(defn run-quantum [initial-game-state]
  ;(->> (iterate quantum-games [[initial-game-state]])
  ;     (take-while has-active-games?)
  ;     (last)
  ;     (quantum-games))
  (->> (quantum-games [[initial-game-state]])
       quantum-games
       quantum-games
       ;quantum-games
       rolls-for-games))

(defn part-2 []
  (->> (assoc game-state :dice-count 1 :dice-size 3 :winning-score 21)
       run-quantum
       get-winners
       frequencies))

(comment
  (def initial-game-state (assoc game-state :dice-count 1 :dice-size 3 :winning-score 10))
  (def results (run-quantum initial-game-state))


  ,)


