(ns advent-2021.day5
  (:require [clojure.string :as s]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(defn parse-coords [coord-string]
  (->> (s/split coord-string #",")
       (map (fn [string-num] (Integer/parseInt string-num)))
       ((fn [[x y]] {:x x :y y}))))

(defn find-line-type [line]
  (let [{start :start end :end} line]
    (if (= (:x start) (:x end)) :vertical
      (if (= (:y start) (:y end)) :horizontal))))

(defn string-to-line [line-string]
  (->> (s/split line-string #" -> ")
       (map parse-coords)
       (sort-by (juxt :x :y))
       ((fn [coords]
          {:start (first coords)
           :end (second coords)}))
       (#(assoc % :type (find-line-type %)))))

(defn load-lines []
  (->> (slurp "resources/lines.txt")
       (s/split-lines)
       (map #(string-to-line %))))

(defn straight-line? [{type :type}] (some? type))

(defn straight-lines []
  (->> (load-lines)
       (filter straight-line?)))

(defn coords-for-line [line]
  (let [{start :start end :end} line]
    (->> (case (:type line)
           :horizontal (map #(do {:x % :y (:y start)})
                            (range (:x start) (inc (:x end))))
           :vertical (map #(do {:x (:x start) :y %})
                          (range (:y start) (inc (:y end))))
           [])
         (set))))

(defn find-intersections-for-lines [line1 line2]
  (->> (set/intersection (coords-for-line line1) (coords-for-line line2))
       (#(if (not-empty %) %))))

(comment
  (->> (straight-lines)
       (find-intersection-points)))

(defn find-intersection-points [lines]
  (->> (reduce
         (fn [all line]
           (let [other-lines (filter #(not= % line) lines)]
             (->> (reduce
                    (fn [all other-line]
                      (->> (find-intersections-for-lines line other-line)
                           (apply conj all)))
                    #{}
                    other-lines)
                  (apply conj all))))
         #{}
         lines)))

(defn line-matches-point? [line point]
  (let [{start :start end :end} line
        horizontal-match?
        (and (<= (:x start) (:x point))
             (>= (:x end) (:x point))
             (= (:y start) (:y point)))
        vertical-match?
        (and (<= (:y start) (:y point))
             (>= (:y end) (:y point))
             (= (:x start) (:x point)))]
    (or horizontal-match? vertical-match?)))

(defn lines-for-point [point lines]
  (filter #(line-matches-point? % point) lines))

(defn part1 []
  (let [lines (straight-lines)
        points (find-intersection-points lines)]
    (->> (filter #(>= (count (lines-for-point % lines)) 2) points)
         (count))))
