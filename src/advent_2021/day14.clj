(ns advent-2021.day14
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))

(def lines (u/read-lines "resources/day14/small.txt"))

(def input (slurp "resources/day14/small.txt"))

(defn parse-input []
  {:polymer (re-find #"^\w+" input)
   :insertion (->> (for [[_ [a b] [c]] (re-seq #"(\w\w) -> (\w)" input)]
                     [[a b] [a c b]])
                   (into {}))})

(def rule-map (:insertion (parse-input)))
(def template (:polymer (parse-input)))

(defn get-template []
  (->> (apply vector (first lines))))
       ;(map str)))

;(def rule-map
;  (->> (split-with #(not (= "" %)) lines)
;       (second)
;       (rest)
;       (map #(str/split % #" -> "))
;       (map (fn [[a b]] {(apply vector a) (first (apply vector b))}))
;       (apply merge-with into)))

(def polymer template)
(def insertion rule-map)

(def counts
  (memoize
    (fn [insertion polymer steps]
      (letfn [(into-counts [result polymer]
                (merge-with +
                            (update result (first polymer) (fnil dec 1))
                            (counts insertion polymer (dec steps))))]

        (if (zero? steps)
          (frequencies polymer)
          (->> (partition 2 1 polymer)
               (map insertion)
               (reduce into-counts {})))))))

(counts rule-map (get-template) 2)

(defn checksum [counts]
  (apply - (apply (juxt max min) (vals counts))))

(defn solution [steps]
    (checksum (counts rule-map (get-template) steps)))

(solution 2)



