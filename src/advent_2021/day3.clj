(ns advent-2021.day3
  (:require [clojure.string :as str]))


(defn part1 [])

(defn- get-report []
  (->>
    (slurp "resources/report.edn")
    (read-string)))

(def small-report
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(def report (get-report))

(defn digits [n]
  (->> n str (map (comp read-string str))))

(defn pivot [dataset]
  (reduce (fn [items item]
            (let [[a b c d e] items
                  [ia ib ic id ie] (digits item)]
              (concat [(conj a ia)
                       (conj b ib)
                       (conj c ic)
                       (conj d id)
                       (conj e ie)])))
          [[]]
          dataset))

(defn most-frequent-bit [bits]
  (key (apply max-key val bits)))

(defn convert-bits [bits]
  (Integer/parseInt (str/join bits) 2))

(defn frequent-bits [input]
  (->>
    (pivot input)
    (map frequencies)
    (map most-frequent-bit)))

(defn gamma [input]
  (->>
    (frequent-bits input)
    (convert-bits)))

(defn epsilon [input]
  (->>
    (frequent-bits input)
    (map #(if (zero? %) 1 0))
    (convert-bits)))



(defn part1 []
  (* (gamma report) (epsilon report)))

(comment
  (pivot report)
  (frequent-bits report)
  (* (gamma report) (epsilon report))
  (get-report)
  (part1))



