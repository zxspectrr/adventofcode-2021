(ns advent-2021.day3
  (:require [clojure.string :as str]))

(defn string-to-digits [n]
  (->> n str (map (comp read-string str))))

(defn- get-report []
  (->>
    (slurp "resources/report.edn")
    (read-string)
    (map string-to-digits)))

(defn pivot [dataset]
  (apply map vector dataset))

(defn calculate-rate [comparator input]
  (->>
    input
    (pivot)
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

(defn find-reading [readings comparator]
  (loop [index 0
         input readings]
    (if (= 1 (count input))
      (str/join (first input))
      (let [{zeros 0 ones 1} (group-by #(nth % index) input)]
        (if (comparator (count zeros) (count ones))
          (recur (inc index) zeros)
          (recur (inc index) ones))))))

(defn part2 []
  (let [input (get-report)
        o2-bits (find-reading input >)
        co2-bits (find-reading input <=)]
    (* (bits-to-decimal o2-bits) (bits-to-decimal co2-bits))))