(ns advent-2021.day14
  (:require [clojure.string :as str]))

(def input (->> (slurp "resources/day14/small.txt")
                (str/split-lines)))

(def template (first input))

(def template-map (->> (rest input)
                       (rest)
                       (map #(str/split % #" -> "))
                       (map (fn [[a b]] {a b}))
                       (apply merge-with into)))

(defn get-val [string-pair]
  (->> (apply str string-pair)
       (get template-map)
       (char-array)
       (first)))

(defn safe-add [existing-value to-add]
  (+ (or existing-value 0) to-add))

(defn step [initial-freq]
  (reduce (fn [acc [[a b] v]]
            (let [c (get-val [a b])]
              (-> acc
                  (update [a c] #(safe-add % v))
                  (update [c b] #(safe-add % v)))))
          {}
          initial-freq))

(defn letter-freqs [template freq-map]
  (reduce (fn [acc [[a b] v]]
            (update acc a #(safe-add % v)))
          {(last template) 1}
          freq-map))

(defn score [letter-freqs]
  (->> (vals letter-freqs)
       (sort)
       ((juxt last first))
       (apply -)))

(defn do-score [steps]
  (->> (partition 2 1 template)
       (frequencies)
       (iterate step)
       (drop steps)
       (first)
       (letter-freqs template)
       (score)))

(defn part1 []
  (do-score 10))

(defn part2 []
  (do-score 40))
