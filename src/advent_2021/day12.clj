(ns advent-2021.day12
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]))

(def lines (u/read-lines "resources/day12-smallest.txt"))

(defn parse-item [item]
  (->> (str/split item #"-")
       ((fn [[f t]] {:from f :to t}))))

(defn build []
  (mapv parse-item lines))

(build)
