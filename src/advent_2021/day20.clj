(ns advent-2021.day20
  (:require [advent-2021.utils :as u]))

(def lines (u/read-lines "resources/day20/input.txt"))
(def lines (u/read-lines "resources/day20/small.txt"))

(def decoder (first lines))

(defn load-image []
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

(defn decode-point [image point]
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

;(defn get-image-height [image]
;  (let [height (count ())]))

(defn get-coords [image]
  (let [width (count (first image))
        height (count image)]
    (->> (for [x (range -2 (+ 2 width))
               y (range -2 (+ 2 height))]
           [x y])
         (sort-by second)
         (partition 9))))

(defn process-image [image]
  ;(->> (map (partial get-pixel image) (get-coords image))
  ;     (partition 15))
  (->> (get-coords image)
       (mapv (fn [line]
               (mapv (fn [coord]
                       (decode-point image coord))
                     line)))))
       ;(map (partial decode-point image))))
       ;(mapv (fn [line]
       ;        (mapv (fn [x] x) line)))))
       ;(map (partial apply str))))

(defn print-image [image]
  (doseq [x (map (partial apply str) image)]
    (prn x))
  image)

(comment

  (def image (load-image))

  (get-coords image)
  (decode-point image [4 2])

  (->> (iterate process-image (load-image))
       (take 1)
       (print-image))

  (print-image (load-image))
  (print-image (process-image (load-image)))

  (decode-point [2 2])
  (->> (find-pixel-plus-neighbours image [2 2])
       (map binary-map)
       (apply str)
       (binary-to-long)
       (map (partial apply str))))


