(ns advent-2021.day8
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "resources/day8.txt")
       (str/split-lines)
       (map #(->>
               (str/split % #" \| ")
               ((fn [[_ digits]]
                  (str/split digits #" ")))))))

(defn collection-contains? [coll value]
  (not (nil? (some #(= value %) coll))))

(defn unique-digit-length? [value]
  (collection-contains? [2 3 4 7] value))

(defn part1 []
  (->> (flatten input)
       (map count)
       (filter unique-digit-length?)
       (count)))



