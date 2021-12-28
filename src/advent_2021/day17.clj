(ns advent-2021.day17)

(def target [[240 292] [-57 90]])
(def target [[20 30] [-5 10]])

(def target-x (set (range 20 31)))
(def target-y (set (range -5 11)))

(defn in-range? [range value] (some range [value]))
