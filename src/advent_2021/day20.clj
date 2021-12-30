(ns advent-2021.day20
  (:require [advent-2021.utils :as u]))

(def lines (u/read-lines "resources/day20/input.txt"))
;(def lines (u/read-lines "resources/day20/small.txt"))

(def decoder (first lines))
(def binary-map {\. 0 \# 1})

(defn load-image []
  {:image (->> (drop 2 lines)
               (mapv (fn [line]
                       (mapv identity line))))
   :void \.})

(defn get-coords [image]
  (let [width (count image)
        r (range -1 (inc width))]
    (->> (for [x r
               y r]
           [x y])
         (sort-by second)
         (partition (+ 2 width)))))

(defn get-pixel [image default [x y]]
  (get-in image [y x] default))

(defn get-point-and-neighbours [[x y]]
  (->> (for [nx [-1 0 1]
             ny [-1 0 1]]
         [(+ nx x) (+ ny y)])
       (sort-by second)))

(defn get-neighbour-pixels [image default [x y]]
  (map (partial get-pixel image default)
       (get-point-and-neighbours [x y])))

(defn parse-binary [str] (Long/parseLong str 2))

(defn process-pixel [image default pixel]
  (->> (get-neighbour-pixels image default pixel)
       (map binary-map)
       (apply str)
       (parse-binary)
       (nth decoder)))

(defn process-line [image default line]
  (mapv (partial process-pixel image default) line))

(defn process-image [{:keys [image void]}]
  {:image (->> (get-coords image)
               (mapv (partial process-line image void)))
   :void  (if (and (= (first decoder) \#)
                   (= \. void))
            \#
            \.)})

(defn print-image [{:keys [image]}]
  (doseq [x image]
    (prn (apply str x))))

(defn process [times]
  (-> (iterate process-image (load-image))
      (nth times)
      :image
      (flatten)
      (frequencies)))

(defn part1 []
  (process 2))

(defn part2 []
  (process 50))