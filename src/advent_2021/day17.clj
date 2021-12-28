(ns advent-2021.day17)

(def target-x (set (range 240 293)))
(def target-y (set (range -90 -56)))
;(def target-x (set (range 20 31)))
;(def target-y (set (range -10 -4)))

(defn in-target-area? [[x y]]
  (some? (and (target-x x)
              (target-y y))))

(defn adjust-velocity [[x y]]
  [(if (> x 0) (dec x) 0)
   (dec y)])

(defn adjust-position [[px py] [vx vy]]
  [(+ px vx) (+ py vy)])

(defn step [{:keys [position velocity]}]
  (let [new-pos (adjust-position position velocity)
        new-velocity (adjust-velocity velocity)]
    {:position new-pos :velocity new-velocity}))

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
       (filter #(in-target-area? (:position %)))
       not-empty
       some?))

(defn y-for-x [x]
  (->> (for [y (range -200 201)]
         (when (winning-throw? [x y])
           [x y]))
       (filter some?)))

(defn max-y-for-x [x]
  (->> (y-for-x x)
       (sort-by second)
       last
       second))

(defn find-highest-velocities []
  (->> (for [x (range 100)]
         (when-let [y (max-y-for-x x)]
           [x y]))
       (filter some?)))

(defn highest-point [[vx vy]]
  (->> (do-throw [vx vy])
       (map #(get-in % [:position 1]))
       (reduce max)))

(defn part1 []
  (->> (map highest-point (find-highest-velocities))
       (reduce max)))

(defn part2 []
  (->> (mapcat y-for-x (range 300))
       (count)))