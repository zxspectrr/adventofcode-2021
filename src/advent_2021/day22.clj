(ns advent-2021.day22
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]))

(def lines (u/read-lines "resources/day22/smaller.txt"))

(defn parse-coord [line]
  (letfn [(get-coord-range [coord-str]
            (->> (str/split coord-str #"\.\.")
                 (mapv (comp u/parse-int (partial re-find #"\d+")))))]

    (let [on-off (->> (str/split line #" ")
                      first
                      (#(if (= "on" %) 1 0)))
          coords (->> (str/split line #" ")
                      second
                      (#(str/split % #","))
                      (mapv get-coord-range))]
      [on-off coords])))

(def input (mapv parse-coord lines))


(comment
  (parse-coord "on x=10..12,y=10..12,z=10..12")
  ,)

(str/split "a..b" #"\.\.")
(re-find #"\d+" "x=10")