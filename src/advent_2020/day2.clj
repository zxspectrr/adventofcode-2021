(ns advent-2020.day2
  (:require [clojure.string :as s]))

(comment
  (map
    #(map (fn [item] (do [(first item) (second item)])) (s/split % #" "))
    (s/split-lines (slurp "resources/coords.txt"))))

(defn- get-coords []
  (let [parse-fn (fn [line]
                   (let [[first second] (s/split line #" ")]
                     [first (Integer/parseInt second)]))]
    (map parse-fn (s/split-lines (slurp "resources/coords.txt")))))

(comment
  (get-coords))

