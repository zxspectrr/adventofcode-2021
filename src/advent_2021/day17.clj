(ns advent-2021.day17)

;(def target [[240 292] [-57 90]])
(def target [[20 30] [-5 -10]])

(def target-x (set (range 240 293)))
(def target-y (set (range -90 -56)))

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
  (let [[x y] position
        min-y (reduce min target-y)
        max-x (reduce max target-x)]
    (> y min-y)))

(defn do-throw [v]
  (let [throws (->> (take-while continue?
                                (iterate step {:position [0 0] :velocity v}))
                    (into []))
        final (step (last throws))]
    (conj throws final)))

(defn winning-throw? [v]
  (->> (do-throw v)
       (filter #(in-range? (:position %)))
       not-empty
       some?))

(defn min-y-for-x [x]
  (->> (for [y (range 0 100)]
         (when (winning-throw? [x y])
           [x y]))
       (filter some?)
       (sort-by second)
       first))

(defn max-y-for-x [x]
  (->> (for [y (range 100 -1 -1)]
         (when (winning-throw? [x y])
           [x y]))
       (filter some?)
       (sort-by second)
       last
       second))

(defn highest-point [[vx vy]]
  (->> (do-throw [vx vy])
       (map #(get-in % [:position 1]))
       (reduce max)))

(defn find-highest-velocities []
  (->> (for [x (range 0 100)]
         (when-let [y (max-y-for-x x)]
           [x y]))
       (filter some?)))

(comment

  (get-in {:position [8 1], :velocity [7 5], :in-range false} [:velocity 1])

  (->> (map highest-point (find-highest-velocities))
       (reduce max))



  (reduce max [9 8 7 6 5 4 3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11])
  [{:position [0 0], :velocity [8 1]}
   {:position [8 1], :velocity [7 0], :in-range false}
   {:position [15 1], :velocity [6 -1], :in-range false}
   {:position [21 0], :velocity [5 -2], :in-range false}
   {:position [26 -2], :velocity [4 -3], :in-range false}
   {:position [30 -5], :velocity [3 -4], :in-range true}
   {:position [33 -9], :velocity [2 -5], :in-range false}
   {:position [35 -14], :velocity [1 -6], :in-range false}]

  (winning-throw? [20 2])
  (do-throw [20 2])

  (->> (for [x (range 0 100)]
         (when-let [y (max-y-for-x x)]
           [x y]))
       (filter some?))
  (max-y-for-x 100)


  (def target [[20 30] [-5 -10]])
  ;(def target [[240 292] [-57 -90]])
 ,)