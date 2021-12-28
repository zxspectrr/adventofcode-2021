(ns advent-2021.day17)

(def target [[240 292] [-57 90]])
(def target [[20 30] [-5 10]])

(def target-x (set (range 20 31)))
(def target-y (set (range -5 11)))

(defn in-range? [range value] (some range [value]))

(defn adjust-velocity [[x y]]
  [(if (> x 0) (dec x) 0)
   (dec y)])

(defn adjust-position [[px py] [vx vy]]
  [(+ px vx) (+ py vy)])

(defn step [[position velocity]]
  (let [new-pos (adjust-position position velocity)
        new-velocity (adjust-velocity velocity)]
    [new-pos new-velocity]))

(take 5 (iterate step [[0 0] [7 2]]))