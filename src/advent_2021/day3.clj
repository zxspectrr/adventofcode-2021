(ns advent-2021.day3
  (:require [clojure.string :as str]))

(defn- get-report []
  (->>
    (slurp "resources/report.edn")
    (read-string)))

(defn string-to-digits [n]
  (->> n str (map (comp read-string str))))

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

(defn find-reading [index readings comparator]
  (let [search-bit
        (as->
          readings v
          (apply map vector v)
          (map frequencies v)
          (nth v index)
          (key (apply comparator val v)))
        filtered (filter #(= (nth % index) search-bit) readings)
        remaining (count filtered)]

    (if (= 1 remaining)
      (str/join (first filtered))
      (find-reading (inc index) filtered comparator))))

(defn part2 []
  (let [pivoted
        (->>
          report
          (map string-to-digits))
        o2-bits (find-reading 0 pivoted max-key)
        co2-bits (find-reading 0 pivoted min-key)]
    (* (bits-to-decimal o2-bits) (bits-to-decimal co2-bits))))

(comment)

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