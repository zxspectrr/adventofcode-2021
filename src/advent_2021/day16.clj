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

(defn parse-literal [binary]
               (let [substr (subs binary 5)
                     literal-chunks (extract-chunks (partition 5 substr))
                     flattened-chunks (flatten literal-chunks)
                     literal-binary (apply str flattened-chunks)
                     bit-length (+ 6 (* 5 (count literal-chunks)))
                     bits (subs binary 0 bit-length)
                     remainder (subs binary bit-length)]
                 {:literal (binary-to-number literal-binary)
                  :bits bits
                  :remainder remainder}))

(comment (parse-literal "000"))

(defn parse-packet [binary]
  (letfn [(parse-literal [binary]
            (let [substr (subs binary 5)
                  literal-chunks (extract-chunks (partition 5 substr))
                  flattened-chunks (flatten literal-chunks)
                  literal-binary (apply str flattened-chunks)
                  bit-length (+ 6 (* 5 (count literal-chunks)))
                  bits (subs binary 0 bit-length)
                  remainder (subs binary bit-length)]
              {:literal (binary-to-number literal-binary)
               :bits bits
               :remainder remainder}))

          (handle-15-bit [binary packets]
            (let [length-bits (subs binary 1 16)
                  sub-packet-length (Integer/parseInt length-bits 2)
                  child-packets (subs binary 16)
                  child-packet (parse-packet child-packets)]
              (parse-packet child-packets)))

          (parse-operator [binary]
            (let [substr (subs binary 6)
                  length-type (first substr)
                  fifteen-bit-length (= \0 length-type)]
              (if fifteen-bit-length
                (handle-15-bit substr)
                false)))]

    (let [header (parse-packet-header binary)
          {:keys [type]} header]

      (if (is-literal? type)
        (parse-literal binary)
        (assoc header :operator (parse-operator binary))))))


(defn part1 []
  (->> (hex-to-binary hex)
       (parse-packet)))

(def hex "8A004A801A8002F478")
(def hex "D2FE28")
(def hex "38006F45291200")

(comment
  (parse-packet "1101000101001010010001001000000000")

  (defn split-packets [binary])

  (hex-to-binary hex)

  (count "101111111000101000"))



