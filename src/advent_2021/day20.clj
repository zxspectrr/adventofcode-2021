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

(defn get-coords [image]
  (let [width (count image)
        padding 1
        r (range (* -1 padding) (+ padding width))]
    (->> (for [x r
               y r]
           [x y])
         (sort-by second)
         (partition (+ (* 2 padding) width)))))

(defn process-image [{:keys [image void]}]
  {:image (->> (get-coords image)
               (mapv (partial process-line image void)))
   :void  (if (and (= (first decoder) \#)
                   (= \. void))
            \#
            \.)})

(defn print-image [image]
  (doseq [x image]
    (prn (apply str x))))

(defn write-image [image]
  (doseq [l image]
    (let [s (str (apply str l) "\n")]
      (spit "resources/day20/output2.txt" s "\n" :append true))))

(defn count-light [image]
  (-> (flatten image)
      (frequencies)
      (get \#)))

(defn process [times]
  (-> (iterate process-image (load-image))
      (nth times)
      :image))

(defn part1 []
  (->> (process 2)
       (count-light)))

(defn part2 []
  (->> (process 50)
       (count-light)))

(comment
  (print-image (:image (load-image)))
  (->> (process 2)
       (write-image)))