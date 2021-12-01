(ns advent-2020.day1-part1)

(defn do-something []
  (get-depths))

(def depths (get-depths))

(defn increased? [current previous]
  (or (nil? previous) (> current previous)))

(defn part1 []
  (->>
    (map-indexed (fn [idx value]
                   (let [previous-index (- idx 1)
                         previous-value (get depths previous-index)]
                     previous-value
                     (if (increased? value previous-value) "increased" "decreased")))
                 depths)
    rest
    (cons "N/A")
    (filter #(= "increased" %))
    (count)))

(comment
  (get-depths))

(defn- get-depths []
  (->>
    (slurp "resources/depths.edn")
    (read-string)))

