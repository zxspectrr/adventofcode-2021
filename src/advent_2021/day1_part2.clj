(ns advent-2020.day1-part2)

(defn- get-depths []
  (->>
    (slurp "resources/depths.edn")
    (read-string)))

(defn- increased? [current previous]
  (or (nil? previous) (> current previous)))

(defn- map-increments [measurements]
  (map #(let [[current next] %]
          (if (increased? next current) "increased" "decreased")) measurements))

(defn part1 []
  (->>
    (get-depths)
    (partition 2 1 depths)
    (map-increments)
    (filter #(= "increased" %))
    (count)))

(defn part2 []
  (->>
    (get-depths)
    (partition 3 1 depths)
    (map #(reduce + %))
    (partition 2 1)
    (map-increments)
    (filter #(= "increased" %))
    (count)))



