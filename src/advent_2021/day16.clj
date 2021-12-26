(ns advent-2021.day16)

(def hex (slurp "resources/day16/input.txt"))

(def hexmap {"0"  "0000", "1"  "0001", "2"  "0010", "3"  "0011", "4"  "0100", "5"  "0101", "6"  "0110", "7"  "0111"
             "8"  "1000", "9"  "1001", "A"  "1010", "B"  "1011", "C"  "1100", "D"  "1101", "E"  "1110", "F"  "1111"})

(defn binary-to-number [binary] (Long/parseLong  binary 2))
(defn get-for-hex-char [char] (get hexmap (str char)))

(defn hex-to-binary [hex]
  (->> (map get-for-hex-char hex) (apply str)))

(defn extract-chunks [potential-chunks]
  (loop [chunks []
         potential-chunks potential-chunks]
    (let [chunk (first potential-chunks)
          prefix (first chunk)
          updated-chunks (conj chunks (rest chunk))]
      (if (= \0 prefix)
        updated-chunks
        (recur updated-chunks (rest potential-chunks))))))

(declare parse-packet)

(defn parse-header [binary]
  (let [version-bits (subs binary 0 3)
        version (binary-to-number version-bits)
        type-bits (subs binary 3 6)
        type (binary-to-number type-bits)]
    {:version version
     :type type
     :binary binary}))

(defn parse-literal [binary]
  (let [substr (subs binary 6)
        literal-chunks (extract-chunks (partition 5 substr))
        flattened-chunks (flatten literal-chunks)
        literal-binary (apply str flattened-chunks)
        bit-length (+ 6 (* 5 (count literal-chunks)))
        remainder (subs binary bit-length)]
    {:type-id :literal
     :value   (binary-to-number literal-binary)
     :remainder (when (not= "" remainder) remainder)}))

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
         (recur (:remainder packet) (conj packets packet) (inc index)))))))

(defn parse-operator [binary]
  (letfn [(handle-15-bit [binary]
            (let [length-bits (subs binary 7 22)
                  sub-packet-length (binary-to-number length-bits)
                  packet-binary (subs binary 22 (+ 22 sub-packet-length))]
              {:type-id :15-bit
               :packets (get-packets packet-binary nil)
               :remainder (subs binary (+ 22 sub-packet-length))}))

          (handle-11-bit [binary]
            (let [length-bits (subs binary 7 18)
                  packet-count (binary-to-number length-bits)
                  packet-string (subs binary 18)
                  packets (get-packets packet-string packet-count)
                  remainder (->> (last packets) (:remainder))]
              {:type-id :11-bit
               :packets packets
               :remainder remainder}))]

    (let [length-type (get binary 6)
          fifteen-bit-length (= \0 length-type)]
      (if fifteen-bit-length
        (handle-15-bit binary)
        (handle-11-bit binary)))))

(defn parse-packet [binary]
  (letfn [(valid-input? [binary] (> (count (frequencies binary)) 1))]

    (when (valid-input? binary)
      (let [header (parse-header binary)
            {:keys [type]} header]

        (if (= type 4)
          (merge header (parse-literal binary))
          (merge header (parse-operator binary)))))))

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

(defn create-reducers [packet]
  (let [{:keys [type-id value packets]} packet]
    (if (= type-id :literal)
      value
      (let [op (get-operator packet)
            args (reduce (fn [acc p] (conj acc (create-reducers p)))
                         []
                         packets)]
        (apply op args)))))

(defn part2 []
  (->> (parse-hex hex)
       (create-reducers)))