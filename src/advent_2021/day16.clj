(ns advent-2021.day16)

(def hex (slurp "resources/day16/input.txt"))

(def hexmap {"0"  "0000", "1"  "0001", "2"  "0010", "3"  "0011", "4"  "0100", "5"  "0101", "6"  "0110", "7"  "0111"
             "8"  "1000", "9"  "1001", "A"  "1010", "B"  "1011", "C"  "1100", "D"  "1101", "E"  "1110", "F"  "1111"})

(defn binary-to-number [binary] (Long/parseLong  binary 2))
(defn get-for-hex-char [char] (get hexmap (str char)))

(defn hex-to-binary [hex]
  (->> (map get-for-hex-char hex) (apply str)))

(defn take-bits [bits length]
  [(subs bits 0 length) (subs bits length)])

(defn parse-bits [bits length]
  (let [[a bits] (take-bits bits length)]
    [(Long/parseLong a 2) bits]))

(declare parse-packet)

(defn get-packets
  ([binary max]
   (get-packets binary 0 max))
  ([binary index max]
   (loop [binary binary
          packets []
          index index]
     (let [packet (parse-packet binary)]
       (if (or (not packet) (= index max))
         packets
         (recur (:remainder packet)
                (conj packets packet)
                (inc index)))))))

(defn parse-length-operator [bits]
  (let [[sub-packet-length bits] (parse-bits bits 15)
        [packet-binary remainder] (take-bits bits sub-packet-length)]
    {:type-id   :15-bit
     :packets   (get-packets packet-binary nil)
     :remainder remainder}))

(defn parse-count-operator [bits]
  (let [[packet-count packet-string] (parse-bits bits 11)
        packets (get-packets packet-string packet-count)
        remainder (->> (last packets) (:remainder))]
    {:type-id   :11-bit
     :packets   packets
     :remainder remainder}))

(defn parse-operator [binary]
  (let [[length-type body] (take-bits binary 1)
        fifteen-bit-length (= "0" length-type)]
    (if fifteen-bit-length
      (parse-length-operator body)
      (parse-count-operator body))))

(defn extract-chunks [potential-chunks]
  (loop [chunks []
         potential-chunks potential-chunks]
    (let [chunk (apply str (first potential-chunks))
          [prefix bits] (take-bits chunk 1)
          updated-chunks (conj chunks bits)]
      (if (= "0" prefix)
        updated-chunks
        (recur updated-chunks (rest potential-chunks))))))

(defn parse-literal [binary]
  (let [chunks (extract-chunks (partition 5 binary))
        combined-chunks (apply str chunks)
        bit-length (* 5 (count chunks))
        [_ remainder] (take-bits binary bit-length)]
    {:type-id   :literal
     :value     (binary-to-number combined-chunks)
     :remainder remainder}))

(defn parse-header [binary]
  (let [[version bits] (parse-bits binary 3)
        [type _] (parse-bits bits 3)]
    {:version version
     :type    type
     :binary  binary}))

(defn parse-packet [binary]
  (letfn [(valid-input? [binary] (get (frequencies binary) \1))]

    (when (valid-input? binary)
      (let [header (parse-header binary)
            [_ body] (take-bits binary 6)
            {:keys [type]} header]
        (if (= type 4)
          (merge header (parse-literal body))
          (merge header (parse-operator body)))))))

(defn flatten-packets [packet]
  (let [{:keys [packets]} packet
        version-map (select-keys packet [:version])
        child-versions
        (if packets
          (->> (reduce (fn [acc cp]
                         (conj acc (flatten-packets cp)))
                       [version-map]
                       packets)
               (flatten))
          version-map)]
    child-versions))

(defn parse-hex [hex]
  (->> (hex-to-binary hex)
       (parse-packet)))

(defn part1 []
  (->> (parse-hex hex)
       (flatten-packets)
       (map :version)
       (reduce +)))

(defn get-operator [{:keys [type]}]
  (case type
    0 +
    1 *
    2 min
    3 max
    5 (fn [first last]
        (cond (nil? first) 0
              (> first last) 1
              :else 0))

    6 (fn [first last]
        (cond (nil? first) 0
              (< first last) 1
              :else 0))

    7 (fn [first last]
        (cond (nil? first) 0
              (= first last) 1
              :else 0))))

(defn calculate-values [packet]
  (let [{:keys [type-id value packets]} packet]
    (if (= type-id :literal)
      value
      (let [op (get-operator packet)
            args (reduce (fn [acc p] (conj acc (calculate-values p)))
                         []
                         packets)]
        (apply op args)))))

(defn part2 []
  (->> (parse-hex hex)
       (calculate-values)))