(ns advent-2021.day6
  (:require [clojure.string :as str]))

(defn load-values []
  (->> (slurp "resources/day6.txt")
       (#(str/split % #","))
       (map #(Integer/parseInt %))))

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

(defn part1 []
  (->> (reduce (fn [acc _] (draw acc))
               input
               (range 0 80))
       (count)))