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
    [(if (= on-off "on") true false) [[x1 (inc x2)]
                                      [y1 (inc y2)]
                                      [z1 (inc z2)]]]))

(defn get-input [lines]
  (mapv parse-line lines))

(defn coord-pairs [& as]
  (->> (sort as)
       (#(map vector % (rest %)))))

(defn inside? [target candidate]
  (let [[[t-min-x t-max-x] [t-min-y t-max-y] [t-min-z t-max-z]] target
        [[c-min-x c-max-x] [c-min-y c-max-y] [c-min-z c-max-z]] candidate]
    (and (<= t-min-x c-min-x c-max-x t-max-x)
         (<= t-min-y c-min-y c-max-y t-max-y)
         (<= t-min-z c-min-z c-max-z t-max-z))))

(defn subtract [target c]
  (let [[[t-min-x t-max-x] [t-min-y t-max-y] [t-min-z t-max-z]] target
        [[c-min-x c-max-x] [c-min-y c-max-y] [c-min-z c-max-z]] c]
    (if (or (> c-min-x t-max-x)
            (> c-min-y t-max-y)
            (> c-min-z t-max-z)
            (< c-max-x t-min-x)
            (< c-max-y t-min-y)
            (< c-max-z t-min-z))
      [target]
      (for [[x1 x2] (coord-pairs t-min-x t-max-x c-min-x c-max-x)
            [y1 y2] (coord-pairs t-min-y t-max-y c-min-y c-max-y)
            [z1 z2] (coord-pairs t-min-z t-max-z c-min-z c-max-z)
            :let [cube [[x1 x2] [y1 y2] [z1 z2]]]
            :when (inside? target cube)
            :when (not (inside? c cube))
            :when (and (not= x1 x2) (not= y1 y2) (not= z1 z2))]
        cube))))

(defn volume [[[min-x max-x] [min-y max-y] [min-z max-z]]]
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