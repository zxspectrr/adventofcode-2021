(ns advent-2021.day3
  (:require [clojure.string :as str]))

(defn- get-report []
  (->>
    (slurp "resources/report.edn")
    (read-string)))

(defn string-to-digits [n]
  (map #(comp read-string str) n))

(defn calculate-rate [comparator input]
  (->>
    input
    (map string-to-digits)
    (apply map vector)
    (map frequencies)
    (map #(key (apply comparator val %)))
    (str/join)))

(defn bits-to-decimal [bits]
  (Integer/parseInt bits 2))

(defn part1 []
  (let [report (get-report)
        gamma (calculate-rate max-key report)
        epsilon (calculate-rate min-key report)]
    (* (bits-to-decimal gamma) (bits-to-decimal epsilon))))

(comment
  (part1))


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