(ns advent-2021.day17)

(def target [[240 292] [-57 90]])

(def target-x (set (range 20 31)))
(def target-y (set (range -10 -4)))

(defn in-range-axis? [range value] (some range [value]))
(defn in-range? [[x y]]
  (and (in-range-axis? target-x x) (in-range-axis? target-y y)))

(defn adjust-velocity [[x y]]
  [(if (> x 0) (dec x) 0)
   (dec y)])

(defn adjust-position [[px py] [vx vy]]
  [(+ px vx) (+ py vy)])

(defn step [{:keys [position velocity]}]
  (let [new-pos (adjust-position position velocity)
        new-velocity (adjust-velocity velocity)]
    {:position new-pos :velocity new-velocity :in-range (some? (in-range? new-pos))}))

(defn continue? [{:keys [position]}]
  (let [[_x y] position
        min-y (reduce min target-y)]
    (> y min-y)))

(defn do-throw [v]
  (let [throws (->> (take-while continue?
                                (iterate step {:position [0 0] :velocity v}))
                    (into []))
        final (step (last throws))]
    (conj throws final)))

(defn winning-throw? [v]
  (->> (do-throw v)
       last
       ;:in-range
       (#(in-range? (:position %)))
       some?))

(comment
  (winning-throw? [6 9])
  (last (do-throw [6 9]))

  (->>
    (take-while continue?
                (iterate step {:position [0 0] :velocity [7 2]}))
    last
    (#(in-range? (:position %)))
    some?)

  (str "test")
 ,)