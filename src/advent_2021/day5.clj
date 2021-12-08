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
      (if (= (:y start) (:y end)) :horizontal
                                  :diagonal))))

(comment
  (filter #(= :diagonal (:type %)) (load-lines)))

(defn string-to-line [line-string]
  (->> (s/split line-string #" -> ")
       (map parse-coords)
       (sort-by (juxt :x :y))
       ((fn [coords]
          {:start (first coords)
           :end (second coords)}))
       (#(assoc % :type (find-line-type %)))))

(defn load-lines []
  (->> (slurp "resources/small-lines.txt")
       (s/split-lines)
       (map #(string-to-line %))))

(defn straight-line? [{type :type}]
  (or (= :vertical type) (= :horizontal type)))

(defn straight-lines []
  (->> (load-lines)
       (filter straight-line?)))

(defn horizontal-coords [start end]
  (map #(do {:x % :y (:y start)})
       (range (:x start) (inc (:x end)))))

(defn vertical-coords [start end]
  (map #(do {:x (:x start) :y %})
       (range (:y start) (inc (:y end)))))

(defn diagonal-coords [start end]
  (let [start-y (:y start)
        end-y (:y end)
        direction (if (pos? (- start-y end-y)) :down :up)]
    (reduce
      (fn [all _]
        (let [last (last all)
              {x :x y :y} last]
          (conj all
                {:x (inc x)
                 :y (if (= direction :up) (inc y) (dec y))})))
      [start]
      (range (:x start) (:x end)))))


(defn coords-for-line [line]
  (let [{start :start end :end} line]
    (->> (case (:type line)
           :horizontal (horizontal-coords start end)
           :vertical (vertical-coords start end)
           :diagonal (diagonal-coords start end)
           []))))
         ;(set))))

(defn find-intersections-for-lines [line1 line2]
  (->> (set/intersection (coords-for-line line1) (coords-for-line line2))
       (#(if (not-empty %) %))))

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

(defn part2 []
  (let [lines (load-lines)
        points (find-intersection-points lines)]
    (->> (filter #(>= (count (lines-for-point % lines)) 2) points)
         (count))))
