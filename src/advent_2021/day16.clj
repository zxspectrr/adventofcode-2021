(ns advent-2021.day16
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-2021.utils :as u]))

(def hex (slurp "resources/day16/input.txt"))
(def hex "8A004A801A8002F478")
(def hex "38006F45291200")
(def hex "D2FE28")

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

(def binary-map (set/map-invert hexmap))
(defn binary-to-number [binary]
  (Integer/parseInt binary 2))

(defn get-for-hex-char [char]
  (get hexmap (str char)))

(defn hex-to-binary [hex]
  (->> (map get-for-hex-char hex)
       (apply str)))

(def binary (hex-to-binary hex))

(defn extract-chunks [potential-chunks]
  (loop [chunks []
         potential-chunks potential-chunks]
    (let [chunk (first potential-chunks)
          prefix (first chunk)
          updated-chunks (conj chunks (rest chunk))]
      (if (= \0 prefix)
        updated-chunks
        (recur updated-chunks (rest potential-chunks))))))

(defn handle-literal [binary]
  (let [literal-chunks (extract-chunks (partition 5 binary))
        flattened-chunks (flatten literal-chunks)
        literal-binary (apply str flattened-chunks)
        remainder (subs binary (* 5 (count literal-chunks)))]
    {:literal (binary-to-number literal-binary)
     :remainder remainder}))

(defn parse-packet-header [binary]
  (let [version-bits (subs binary 0 3)
        version (binary-to-number version-bits)
        type-bits (subs binary 3 6)
        type (binary-to-number type-bits)]
    {:version version
     :type    type
     :literal (handle-literal (subs binary 6))}))

(defn part1 []
  (->> (hex-to-binary hex)
       (parse-packet-header)))


(defn split-packets [binary])

(hex-to-binary hex)
