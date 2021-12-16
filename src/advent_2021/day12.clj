(ns advent-2021.day12
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]))

(def lines (u/read-lines "resources/day12-smallest.txt"))

(defn parse-item [item]
  (->> (str/split item #"-")
       (mapv keyword)))

(defn build-nodes []
  (mapv parse-item lines))

(defn find-nodes-from [nodes node]
  (filter (fn [[a _]] (= node a)) nodes))

(defn find-nodes-to [nodes node]
  (filter (fn [[_ b]] (= node b)) nodes))

(defn find-start [nodes] (find-nodes-from nodes :start))
(defn find-end [nodes] (find-nodes-to nodes :end))

(defn multi-pass? [node]
  (->> (str node) (#(= (str/upper-case %) %))))

(def nodes (build-nodes))

(find-nodes-from nodes :start)

(defn walk [nodes from walked]
  (reduce (fn [w [f t]]
            (conj w [f t]))
          walked
          (find-nodes-from nodes from)))

(defn start [nodes]
  (walk nodes :start #{}))

(find-nodes-from nodes :A)

nodes

(filter multi-pass? nodes)

(comment
  (find-end nodes)
  (find-start nodes))