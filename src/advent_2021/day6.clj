(ns advent-2021.day6
  (:require [clojure.string :as str]))

(defn parse-long [input-string]
  (-> input-string Double/parseDouble long))

(defn load-values []
  (->> (slurp "resources/day6-small.txt")
       (#(str/split % #","))
       (map parse-long)))

(defn replace-zero [input]
  (if (= 0 input) 7 input))

(defn create-new-fish [fish]
  (-> (frequencies fish)
      (#(or (get % 0) 0))
      (repeat 9)))

(defn draw [input]
  (->>
    (create-new-fish input)
    (concat input)
    (map replace-zero)
    (map dec)))

(def input (load-values))

(defn calculate-days [days]
  (->> (reduce (fn [acc _] (draw acc))
             input
             (range 0 days))
     (count)))

(defn part1 []
  (calculate-days 80))

(defn part2 []
  (calculate-days 256))
