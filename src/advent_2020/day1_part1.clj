(ns advent-2020.day1-part1)

(def depths (get-depths))

(defn- increased? [current previous]
  (or (nil? previous) (> current previous)))

(defn map-to-increments
  ([measurements]
   (map-to-increments (first measurements) (rest measurements) ["N/A"]))

  ([previous items results]
   (let [current (first items)
         others (rest items)
         value (if (increased? current previous) "increased" "decreased")
         new-results (conj results value)]
     (if (empty? others)
       new-results
       (map-to-increments current others new-results)))))

(defn part1 []
  (->>
    (map-to-increments depths)
    (filter #(= "increased" %))
    (count)))

(comment
  (get-depths))

(defn- get-depths []
  (->>
    (slurp "resources/depths.edn")
    (read-string)))

