(ns advent-2021.day3
  (:require [clojure.string :as str]))

(defn- get-report []
  (->>
    (slurp "resources/report.edn")
    (read-string)))

(defn digits [n]
  (->> n str (map (comp read-string str))))

(defn bits-to-number [bits]
  (->
    (str/join bits)
    (Integer/parseInt 2)))

(defn pivot [digits]
  (map-indexed (fn [index _]
                 (map #(nth % index) digits))
               (first digits)))

(defn most-frequent-bit [bits]
  (key (apply max-key val bits)))

(defn frequent-bits [input]
  (->>
    (map digits input)
    (pivot)
    (map frequencies)
    (map most-frequent-bit)))

(defn gamma [input]
  (->>
    (frequent-bits input)
    (bits-to-number)))

(defn epsilon [input]
  (->>
    (frequent-bits input)
    (map #(if (zero? %) 1 0))
    (bits-to-number)))

(defn power [input]
  (* (gamma input) (epsilon input)))

(defn part1 []
  (power (get-report)))

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

(comment
  (part1))




