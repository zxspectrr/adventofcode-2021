(ns advent-2021.day9
  (:require [clojure.string :as str]))

(defn parse-int [str]
  (Integer/parseInt str))

(def input
  (->> (slurp "resources/day9-small.txt")
       (str/split-lines)
       (map #(->> (into [] (map parse-int (re-seq #"\d" %)))))
       (into [])))

(defn build-neighbour-map [input]
  (map-indexed
    (fn [idx line]
      (map-indexed
        (fn [idx-inner point]
          (let [upper (get-in input [(dec idx) idx-inner])
                lower (get-in input [(inc idx) idx-inner])
                left (get line (dec idx-inner))
                right (get line (inc idx-inner))]
            {:point point :neighbours (remove nil? [upper lower left right])}))
        line))
    input))

(defn smaller-than-neighbours? [point neighbours]
  (< point (reduce min neighbours)))

(defn find-smallest-points [grid-line]
  (->> (filter (fn [{:keys [point neighbours]}]
                 (smaller-than-neighbours? point neighbours))
               grid-line)
       (map (fn [{:keys [:point]}] point))))

(defn part1 []
  (->>
    (build-neighbour-map input)
    (mapcat find-smallest-points)
    (map inc)
    (reduce +)))

(defn second-to-last [coll]
  (nth coll (dec (dec (count coll)))))

(def line (first input))

(defn pivot [dataset] (apply map vector dataset))

;(defn low-points-in-line [line]
;  (->>
;    (second-to-last line)
;    (conj line)
;    (partition 2 1)
;    (filter (fn [[a b]] (< a b)))
;    (map (fn [[a _]] a))))

(defn low-points-in-line [line]
  (->>
    (second-to-last line)
    (conj line)
    (map-indexed (fn [idx x]
                   [(get line (dec idx))
                    x
                    (get line (inc idx))]))
    (filter (fn [[p x n]]
              (and (or (nil? p)
                       (< x p))
                   (or (nil? n)
                       (< x n)))))
    (map (fn [[_ x _]] x))))

(defn is-low-point? [line point]
  (as-> (low-points-in-line line) _
        (into #{} _)
        (_ point)
        (some? _)))

(defn find-vertical-line [idx lines]
  (nth (pivot lines) idx))

(defn is-2d-low-point? [line-number index point lines]
  (let [horizontal-line (get lines line-number)
        vertical-line (find-vertical-line index lines)]
    (and (is-low-point? horizontal-line point)
         (is-low-point? vertical-line point))))

(def vert-line [2 3 9 8 9])

(partition-all 2 1 vert-line)

(->>
  (second-to-last vert-line)
  (conj vert-line)
  (partition 3 1))
  (filter (fn [[a b]] (< a b)))
  (map (fn [[a _]] a))))

(low-points-in-line [2 3 9 8 9])

(find-vertical-line 0 lines)

(def input2
  (->> (slurp "resources/day9-small.txt")
       str/split-lines
       (mapv (fn [line]
               (mapv (comp-> str parse-long) line)))))

(def file-lines (slurp "resources/day9-small.txt"))

(for [x (range (count (first input)))
      y (range (count input))]
  [y x]))

(defn- grid [input]
  (take 10
        (for [x (range (count (first input)))
              y (range (count input))]
          [y x])))

(for [position (grid input)
      :let [h (get-in input position)]]
           h)




(map-indexed
  (fn [line-number line]
    (map-indexed
      (fn [idx point]
        (if (is-2d-low-point? line-number idx point input)
          point
          nil))
      line))
  input)



  (defn low-points-for-line [line]
    ;(letfn [(second-to-last [col])]))

    )


      ;(->> (map low-points-for-line input)
      ;     (map inc)
      ;     (reduce +))
      ;
      ;(->>
      ;  (second-to-last test-line)
      ;  (conj test-line)
      ;  (partition 2 1)
      ;  (filter (fn [[a b]] (< a b)))
      ;  (map (fn [[a _]] (inc a)))
      ;  (count))
      ;
      ;
      ;(str "test")))