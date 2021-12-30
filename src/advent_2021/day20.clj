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
        height (count image)
        padding 2]
    (->> (for [x (range (flip padding) (+ padding width))
               y (range (flip padding) (+ padding height))]
           [x y])
         (sort-by second)
         (partition (+ (* 2 padding) width)))))

(defn process-line [image line]
  (mapv (partial decode-point image) line))

(defn process-image [image]
  (->> (get-coords image)
       (mapv #(process-line image %))))

(defn print-image [image]
  (doseq [x (map (partial apply str) image)]
    (prn x)))

(comment
  (print-image (process-image (load-image)))
  (->> (mapv (fn [line]
               (mapv (fn [x] (get-pixel image x)) line))
             (get-coords image))
       (print-image))

  (get-coords image)

  (def image (load-image))

  (get-coords image)
  (decode-point image [1 2])

  (get-pixel image [1 2])

  (->> (iterate process-image (load-image))
       (take 1)
       (print-image))

  (print-image (load-image))

  (print-image (get-coords image))


  (decode-point [2 2])
  (->> (find-pixel-plus-neighbours image [2 2])
       (map binary-map)
       (apply str)
       (binary-to-long)
       (map (partial apply str))))


