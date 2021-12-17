(ns advent-2021.day14
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]))

(def lines (u/read-lines "resources/day14/small.txt"))

(defn get-template []
  (->> (apply vector (first lines))
       (map str)))

(def rule-map
  (->> (split-with #(not (= "" %)) lines)
       (second)
       (rest)
       (map #(str/split % #" -> "))
       (into {})))

(defn get-element [char-arr]
  (get rule-map (reduce str char-arr)))

(defn add-middle-element [[first second :as char-arr]]
  (->> (get-element char-arr)
       ((fn [element] [first element second]))))

(defn process-char-pair [results char-pair]
  (let [updated (add-middle-element char-pair)
        result (if (empty? results) updated
                                    (rest updated))]
    (conj results result)))

(defn step [template]
  (->> (reduce process-char-pair []
               (partition 2 1 template))
       (flatten)))

(->> (take 11 (iterate step (get-template)))
     (last)
     (count))


