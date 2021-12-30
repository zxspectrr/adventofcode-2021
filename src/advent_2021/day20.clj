(ns advent-2021.day20
  (:require [advent-2021.utils :as u]))

(def lines (u/read-lines "resources/day20/input.txt"))
;(def lines (u/read-lines "resources/day20/small.txt"))

(def decoder (first lines))

(defn load-image []
  {:image (mapv (fn [line]
                  (mapv (fn [c] c) line))
                (drop 2 lines))
   :void \.})

(defn get-pixel [image [x y] default]
  (get-in image [y x] default))

(defn find-pixel-plus-neighbours [image [x y] void]
  (let [adjustments [[-1 -1] [0 -1] [1 -1]
                     [-1 0] [0 0] [1 0]
                     [-1 1] [0 1] [1 1]]]
    (->> (map (fn [[ax ay]]
                [(+ x ax) (+ y ay)]) adjustments)
         (map #(get-pixel image % void)))))

(def binary-map {\. "0" \# "1"})
(def decode-number (partial nth decoder))

(defn binary-to-long [binary-str]
  (Long/parseLong binary-str 2))

(defn decode-point [image point void]
  (->> (find-pixel-plus-neighbours image point void)
       (map binary-map)
       (apply str)
       (binary-to-long)
       (decode-number)))

(defn get-coords [image]
  (let [width (count (first image))
        r (range -1 (inc width))]
    (->> (for [x r
               y r]
           [x y])
         (sort-by second)
         (partition (+ 2 width)))))

(defn process-line [image line void]
  (mapv #(decode-point image % void) line))

(defn process-image [{:keys [image void]}]
  {:image (->> (get-coords image)
               (mapv #(process-line image % void)))
   :void (if (and (= \. void)
                  (= (first decoder) \#))
           \#
           \.)})

(defn print-image [image]
  (doseq [x (map (partial apply str) image)]
    (prn x)))

(defn process-times [n]
  (-> (iterate process-image (load-image))
      (nth n)
      :image
      flatten
      frequencies
      (#(get % \#))))

(defn part1 []
  (process-times 2))

(defn part2 []
  (process-times 50))




