(ns advent-2021.day20
  (:require [advent-2021.utils :as u]))

(def lines (u/read-lines "resources/day20/input.txt"))
(def lines (u/read-lines "resources/day20/small.txt"))

(def decoder (first lines))

(def image
  (mapv (fn [line]
          (mapv (fn [c] c) line))
        (drop 2 lines)))

(defn get-pixel [image [x y]]
  (if-let [p (get-in image [y x])]
    p
    \.))

(defn find-pixel-plus-neighbours [image [x y]]
  (let [adjustments [[-1 -1] [0 -1] [1 -1]
                     [-1 0] [0 0] [1 0]
                     [-1 1] [0 1] [1 1]]]
    (->> (map (fn [[ax ay]]
                [(+ x ax) (+ y ay)]) adjustments)
         (map (partial get-pixel image)))))

(def binary-map {\. "0" \# "1"})

(def decode-number (partial nth decoder))

(defn binary-to-long [binary-str]
  (Long/parseLong binary-str 2))

(defn decode-point [point]
  (->> (find-pixel-plus-neighbours image point)
       (map binary-map)
       (apply str)
       (binary-to-long)
       (decode-number)))

(def flip (partial * -1))
(def double* (partial * 2))

(defn get-image-width [image]
  (let [width (count (first image))]
    (* 3 width)))

(get-image-width image)

(defn get-coords [image]
  (let [width (count (first image))
        height (count image)]
    (for [x (range (flip width) (double* width))
          y (range (flip height) (double* height))]
      [x y])))

(defn process-image [image]
  (->> (map decode-point (get-coords image))
       (partition (get-image-width image))))


(comment


  (decode-point [2 2])
  (->> (find-pixel-plus-neighbours image [2 2])
       (map binary-map)
       (apply str)
       (binary-to-long)))


