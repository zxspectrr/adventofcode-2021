(ns advent-2021.day1-part1)

(def depths (get-depths))

(defn- increased? [current previous]
  (or (nil? previous) (> current previous)))

(defn part1 []
  (->>
    (partition 2 1 depths)
    (map #(let [[current next] %]
            (if (increased? next current) "increased" "decreased")))
    (filter #(= "increased" %))
    (count)))

(comment
  (->>
    (partition 2 1 depths)
    (take 5))
    ;(map #(let [[first second] %]
    ;        (if (increased? second first) "increased" "decreased")))
    ;(filter #(= "increased" %))
    ;(count))
  (get-depths))

(defn- get-depths []
  (->>
    (slurp "resources/depths.edn")
    (read-string)))

