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

(defn parse-literal [binary]
  (let [substr (subs binary 6)
        literal-chunks (extract-chunks (partition 5 substr))
        flattened-chunks (flatten literal-chunks)
        literal-binary (apply str flattened-chunks)
        bit-length (+ 6 (* 5 (count literal-chunks)))
        remainder (subs binary bit-length)]
    {:literal   (binary-to-number literal-binary)
     :remainder (when (not= "" remainder) remainder)}))

(defn get-packets [binary]
  (loop [binary binary
         packets []]
    (let [packet (parse-packet binary)]
      (if (not packet)
        packets
        (recur (:remainder packet) (conj packets packet))))))

(defn parse-packet [binary]
  (letfn [(is-literal? [type-id] (= 4 type-id))
          (invalid-input? [binary] (< (count (frequencies binary)) 2))
          (valid-input? [binary] (not (invalid-input? binary)))
          (handle-15-bit [binary]
            (let [length-bits (subs binary 7 22)
                  sub-packet-length (binary-to-number length-bits)
                  packet-binary (subs binary 22 (+ 22 sub-packet-length))]
              {:operator-type :15-bit
               :packets (get-packets packet-binary)
               :remainder (subs binary (+ 22 sub-packet-length))}))

          (handle-11-bit [binary]
            (let [length-bits (subs binary 7 18)
                  packet-count (binary-to-number length-bits)
                  packet-string (subs binary 18)
                  packets (get-packets packet-string)]
                  ;packets-as-chars (if (> packet-count 1)
                  ;                   (partition 11 packet-string)
                  ;                   (vector (vec packet-string)))
                  ;packets-as-binary (map #(apply str %) packets-as-chars)]
              {:operator-type :11-bit
               :packets packets}))

          (parse-operator [binary]
            (let [length-type (get binary 6)
                  fifteen-bit-length (= \0 length-type)]
              (if fifteen-bit-length
                (handle-15-bit binary)
                (handle-11-bit binary))))]

    (when (valid-input? binary)
      (let [header (parse-packet-header binary)
            {:keys [type]} header]

        (if (is-literal? type)
          (merge header (parse-literal binary))
          (merge header (parse-operator binary)))))))

(defn flatten-packets [packet]
  (let [{:keys [packets]} packet
        version-map (select-keys packet [:version])
        child-versions
        (if packets
          (reduce (fn [acc cp]
                    (conj acc (flatten-packets cp)))
                  [version-map]
                  packets)
          version-map)]
    child-versions))

(defn parse-hex [hex]
  (->> (hex-to-binary hex)
       (parse-packet)))

(defn part1 []
  (->> (parse-hex "C0015000016115A2E0802F182340")
       (flatten-packets)
       (flatten)
       (map :version)
       (reduce +)))

(comment

  (def hex "8A004A801A8002F478")
  (def hex "D2FE28")
  (def hex "38006F45291200"))

(comment
  (parse-packet "1101000101001010010001001000000000")
  (defn split-packets [binary])
  (hex-to-binary hex)
  (count "101111111000101000"))



