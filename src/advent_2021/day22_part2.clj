(ns advent-2021.day22-part2
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(def lines (u/read-lines "resources/day22/input.txt"))

(defn parse-coord [line]
  (letfn [(get-coord-range [coord-str]
            (->> (str/split coord-str #"=")
                 second
                 (#(mapv u/parse-int (str/split % #"\.\.")))))]

    (let [on-off (->> (str/split line #" ")
                      first
                      (#(if (= "on" %) true false)))
          coords (->> (str/split line #" ")
                      second
                      (#(str/split % #","))
                      (mapv get-coord-range))]
      [on-off coords])))

(defn coord-pairs [& as]
  (->> (sort as)
       (#(map vector % (rest %)))))

(defn inside? [target candidate]
  (let [[[minx1 maxx1] [miny1 maxy1]] target
        [[minx2 maxx2] [miny2 maxy2]] candidate]
    (and (<= minx1 minx2 maxx2 maxx1)
         (<= miny1 miny2 maxy2 maxy1))))

(defn subtract [target c]
  (let [[[minx1 maxx1] [miny1 maxy1]] target
        [[minx2 maxx2] [miny2 maxy2]] c]
    (->> (for [[x1 x2] (coord-pairs minx1 maxx1 minx2 maxx2)
               [y1 y2] (coord-pairs miny1 maxy1 miny2 maxy2)
               :let [cube [[x1 x2] [y1 y2]]]
               :when (inside? target cube)
               :when (not (inside? c cube))]
           cube)
         (filter (fn [[[a b] [c d]]] (and (not= a b)
                                          (not= c d)))))))

(defn volume [[[minx maxx] [miny maxy]]]
  (* (- maxx minx)
     (- maxy miny)))

(subtract [[1 4] [1 4]] [[1 4] [2 5]])

(defn subtract-cubes [cubes]
  (reduce (fn [acc [on-off cube]]
            (let [result (mapcat (fn [c]
                                   (subtract c cube)) acc)]
              (if on-off
                (conj result cube)
                result)))
          []
          cubes))

(subtract-cubes [[true [[1 4] [1 4]]]
                 [false [[3 5] [3 6]]]
                 [true [[1 2] [1 4]]]])

(defn get-input [lines]
  (mapv parse-coord lines))

(count (subtract-cubes (get-input lines)))


(defn part2 []
  (->> (get-input lines)))