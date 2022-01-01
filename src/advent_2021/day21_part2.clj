(ns advent-2021.day21-part2)

(def next-turn (partial {0 1 1 0}))

(defn move [position roll]
  (let [score (dec (+ position roll))]
    (nth (cycle (range 1 11)) score)))

(defn find-winner [{:keys [scores]}]
  (cond (>= (get scores 0) 21) 0
        (>= (get scores 1) 21) 1
        :else nil))

(defn update-game-state [{:keys [turn positions] :as state} roll]
  (let [pos (get positions turn)
        new-pos (move pos roll)]
    (-> (update state :turn next-turn)
        (assoc-in [:positions turn] new-pos)
        (update-in [:scores turn] + new-pos))))

(def quantum-frequencies
  (frequencies (let [rolls [1 2 3]]
                 (for [a rolls b rolls c rolls]
                   (+ a b c)))))

(defn step-game [game-state existing-weight]
  (reduce (fn [state-map [r weight]]
            (merge-with + state-map
                        { (update-game-state game-state r) (* existing-weight weight)}))
          {}
          quantum-frequencies))

(defn step-world [{:keys [games] :as world}]
  (reduce (fn [games-state [game-state count]]
            (let [updated-state (group-by (fn [[k _]] (find-winner k))
                                          (step-game game-state count))
                  {p1 0 p2 1 remain nil} updated-state]
              (-> games-state
                  (update-in [:win-count 0] + (reduce + (map second p1)))
                  (update-in [:win-count 1] + (reduce + (map second p2)))
                  (update :games dissoc game-state)
                  (update :games #(merge-with + % (into {} remain))))))
          world
          games))

(def world-state {:games     {{:positions [1 2] ; [4 8]
                               :scores    [0 0]
                               :turn      0
                               :winner    nil} 1}
                  :win-count [0 0]})

(defn part2 []
  (->> (iterate step-world world-state)
       (drop-while #(seq (:games %)))
       first
       :win-count
       (sort)
       last))