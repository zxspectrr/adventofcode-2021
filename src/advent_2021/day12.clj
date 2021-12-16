(ns advent-2021.day12
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]))

(def lines (u/read-lines "resources/day12-smallest.txt"))

(def lower-case? (complement u/uppercase?))

(defn parse-graph [lines]
  (apply merge-with into
         (for [line lines
               :let [[a b] (str/split line #"-")]]
           {a [b], b [a]})))

(defn small-cave? [node] (lower-case? node))

(def graph (parse-graph lines))

(defn dfs-paths [graph goal path allowances]
  (let [curr (peek path)]
    (if (= goal curr)
      (vector path)
      (let [nexts (filter #(pos? (get allowances %)) (get graph curr))]
        (mapcat #(dfs-paths graph goal (conj path %) (update allowances curr dec)) nexts)))))

(defn make-allowances [graph]
  (let [{small true, big false} (group-by small-cave? (keys graph))]
    (merge (zipmap small (repeat 1)) (zipmap big (repeat ##Inf)))))

(defn part1 []
  (let [graph (parse-graph lines)
        allowances (make-allowances graph)]
    (count (dfs-paths graph "end" ["start"] allowances))))
