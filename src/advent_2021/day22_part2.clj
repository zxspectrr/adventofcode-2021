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
    [(if (= on-off "on") true false) [[x1 (inc x2)] [y1 (inc y2)] [z1 (inc z2)]]]))

(defn get-input [lines]
  (mapv parse-line lines))

(defn coord-pairs [& as]
  (->> (sort as)
       (#(map vector % (rest %)))))

(defn inside? [target candidate]
  (let [[[minx-t maxx-t] [miny-t maxy-t] [minz-t maxz-t]] target
        [[minx-c maxx-c] [miny-c maxy-c] [minz-c maxz-c]] candidate]
    (and (<= minx-t minx-c maxx-c maxx-t)
         (<= miny-t miny-c maxy-c maxy-t)
         (<= minz-t minz-c maxz-c maxz-t))))

(defn subtract [target c]
  (let [[[minx-t maxx-t] [miny-t maxy-t] [minz-t maxz-t]] target
        [[minx-c maxx-c] [miny-c maxy-c] [minz-c maxz-c]] c]
    (if (or (> minx-c maxx-t)
            (> miny-c maxy-t)
            (> minz-c maxz-t)
            (< maxx-c minx-t)
            (< maxy-c miny-t)
            (< maxz-c minz-t))
      [target]
      (->> (for [[x1 x2] (coord-pairs minx-t maxx-t minx-c maxx-c)
                 [y1 y2] (coord-pairs miny-t maxy-t miny-c maxy-c)
                 [z1 z2] (coord-pairs minz-t maxz-t minz-c maxz-c)
                 :let [cube [[x1 x2] [y1 y2] [z1 z2]]]
                 :when (inside? target cube)
                 :when (not (inside? c cube))]
             cube)
           (filter (fn [[[a b] [c d] [e f]]]
                     (and (not= a b) (not= c d) (not= e f))))))))

(defn volume [[[minx maxx] [miny maxy] [minz maxz]]]
  (* (- maxx minx)
     (- maxy miny)
     (- maxz minz)))

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