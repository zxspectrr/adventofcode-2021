(ns advent-2021.day22-part2
  (:require [advent-2021.utils :as u]))

(def lines (u/read-lines "resources/day22/input.txt"))
(def small-lines (u/read-lines "resources/day22/smaller.txt"))
(def example-lines (u/read-lines "resources/day22/example.txt"))

(defn parse-line [line]
  (let [[_ on-off & coordstr] (re-matches
                                #"(on|off) x=([0-9-]+)..([0-9-]+),y=([0-9-]+)..([0-9-]+),z=([0-9-]+)..([0-9-]+)"
                                line)
        [x1 x2 y1 y2 z1 z2] (map u/parse-long coordstr)]
    [(if (= on-off "on") true false) {:min-x x1 :max-x (inc x2)
                                      :min-y y1 :max-y (inc y2)
                                      :min-z z1 :max-z (inc z2)}]))

(defn get-input [lines]
  (mapv parse-line lines))

(defn coord-pairs [& as]
  (->> (sort as)
       (#(map vector % (rest %)))))

(defn inside? [target candidate]
  (and (<= (:min-x target) (:min-x candidate) (:max-x candidate) (:max-x target))
       (<= (:min-y target) (:min-y candidate) (:max-y candidate) (:max-y target))
       (<= (:min-z target) (:min-z candidate) (:max-z candidate) (:max-z target))))

(defn subtract [target candidate]
  (if (or (> (:min-x candidate) (:max-x target))
          (> (:min-y candidate) (:max-y target))
          (> (:min-z candidate) (:max-z target))
          (< (:max-x candidate) (:min-x target))
          (< (:max-y candidate) (:min-y target))
          (< (:max-z candidate) (:min-z target)))
    [target]
    (for [[x1 x2] (coord-pairs (:min-x target) (:max-x target) (:min-x candidate) (:max-x candidate))
          [y1 y2] (coord-pairs (:min-y target) (:max-y target) (:min-y candidate) (:max-y candidate))
          [z1 z2] (coord-pairs (:min-z target) (:max-z target) (:min-z candidate) (:max-z candidate))
          :let [cube {:min-x x1 :max-x x2 :min-y y1 :max-y y2 :min-z z1 :max-z z2}]
          :when (inside? target cube)
          :when (not (inside? candidate cube))
          :when (and (not= x1 x2) (not= y1 y2) (not= z1 z2))]
      cube)))

(defn volume [{:keys [min-x max-x min-y max-y min-z max-z]}]
  (* (- max-x min-x)
     (- max-y min-y)
     (- max-z min-z)))

(defn subtract-cubes [cubes]
  (reduce (fn [acc [on-off cube]]
            (let [result (mapcat (fn [c]
                                   (subtract c cube)) acc)]
              (if on-off
                (conj result cube)
                result)))
          []
          cubes))

(defn part2 []
  (->> (get-input lines)
       subtract-cubes
       (map volume)
       (reduce +)))