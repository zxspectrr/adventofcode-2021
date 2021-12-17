(ns advent-2021.day14
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(def lines (u/read-lines "resources/day14/small.txt"))

(defn get-template []
  (->> (apply vector (first lines))))
       ;(map str)))

(def rule-map
  (->> (split-with #(not (= "" %)) lines)
       (second)
       (rest)
       (map #(str/split % #" -> "))
       (map (fn [[a b]] {(apply vector a) (first (apply vector b))}))
       (apply merge-with into)))

(defn get-element [char-arr]
  (get rule-map char-arr))

(defn add-middle-element [[first second :as char-arr]]
  (->> (get-element char-arr)
       ((fn [element] [first element second]))))

(defn process-char-pair [results char-pair]
  (let [updated (add-middle-element char-pair)
        result (if (empty? results) updated
                                    (rest updated))]
    (into results result)))

(defn step [template]
  (->> (reduce process-char-pair []
               (partition 2 1 template))))
       ;(flatten)))

(defn score [sequence]
  (let [freqs (frequencies sequence)
        max-val (reduce max (vals freqs))
        min-val (reduce min (vals freqs))]
    (- max-val min-val)))


(defn part1 []
  (->> (take 11 (iterate step (get-template)))
       (last)
       (score)))

(defn part2 []
  (->> (take 41 (iterate step (get-template)))
       (last)
       (score)))



