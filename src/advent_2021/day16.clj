(ns advent-2021.day16
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-2021.utils :as u]))

(def hex (slurp "resources/day16/input.txt"))

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

(defn is-literal? [type-id] (= 4 type-id))

(defn binary-to-number [binary] (Integer/parseInt binary 2))

(defn get-for-hex-char [char] (get hexmap (str char)))

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

(defn parse-packet-header [binary]
  (let [version-bits (subs binary 0 3)
        version (binary-to-number version-bits)
        type-bits (subs binary 3 6)
        type (binary-to-number type-bits)]
    {:version version :type type}))


(declare parse-packet)

(comment
 (parse-packet "110100010100101001000100100")
 (parse-packet "0101001000100100"))

(defn parse-literal [binary]
               (let [substr (subs binary 6)
                     literal-chunks (extract-chunks (partition 5 substr))
                     flattened-chunks (flatten literal-chunks)
                     literal-binary (apply str flattened-chunks)
                     bit-length (+ 6 (* 5 (count literal-chunks)))
                     remainder (subs binary bit-length)]
                 {:literal   (binary-to-number literal-binary)
                  :remainder (if (not= "" remainder) remainder)}))

(defn get-packets [binary]
  (loop [binary binary
         packets []]
    (let [packet (parse-packet binary)
          {:keys [remainder]} packet
          result (conj packets packet)]
      (if (not remainder)
        result
        (recur remainder result)))))

(defn parse-packet [binary]
  (letfn [(handle-15-bit [binary]
            (let [length-bits (subs binary 7 22)
                  sub-packet-length (Integer/parseInt length-bits 2)
                  packet-binary (subs binary 22 (+ 22 sub-packet-length))]
              {:packets (get-packets packet-binary)}))

          (handle-11-bit [binary]
            (let [length-bits (subs binary 7 18)
                  packet-count (Integer/parseInt length-bits 2)
                  subs-end (+ 18 (* 11 packet-count))
                  packet-string (subs binary 18 subs-end)
                  packets-as-chars (partition 11 packet-string)
                  packets-as-binary (map #(apply str %) packets-as-chars)]
              {:packets (map parse-packet packets-as-binary)}))

          (parse-operator [binary]
            (let [length-type (get binary 5)
                  fifteen-bit-length (= \0 length-type)]
              (if fifteen-bit-length
                (handle-15-bit binary)
                (handle-11-bit binary))))]

    (let [header (parse-packet-header binary)
          {:keys [type]} header]

      (if (is-literal? type)
        (merge header (parse-literal binary))
        (merge header (parse-operator binary))))))


(defn part1 []
  (->> (hex-to-binary "EE00D40C823060")
       (parse-packet))


  true)

(def hex "8A004A801A8002F478")
(def hex "D2FE28")
(def hex "38006F45291200")

(comment
  (parse-packet "1101000101001010010001001000000000")
  (defn split-packets [binary])
  (hex-to-binary hex)
  (count "101111111000101000"))



