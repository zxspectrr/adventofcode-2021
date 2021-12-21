(ns advent-2021.day16
  (:require [clojure.string :as str]
            [advent-2021.utils :as u]))

(def hex (slurp "resources/day16/input.txt"))
(def hex "8A004A801A8002F478")

(def hexmap {"0"  "0000"
             "1"  "0001"
             "2"  "0010"
             "3"  "0011"
             "4"  "0100"
             "5"  "0101"
             "6"  "0110"
             "7"  "0111"
             "8"  "1000"
             "9"  "1001"
             "A"  "1010"
             "B"  "1011"
             "C"  "1100"
             "D"  "1101"
             "E"  "1110"
             "F"  "1111"})

(defn get-for-hex-char [char]
  (get hexmap (str char)))

(defn hex-to-binary [hex]
  (->> (map get-for-hex-char hex)
       (apply str)))

(hex-to-binary hex)





  (def hex "8A004A801A8002F478")
