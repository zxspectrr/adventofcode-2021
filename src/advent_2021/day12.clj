(ns advent-2021.day12
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]))

(def lines (u/read-lines "resources/day12-smallest.txt"))

(defn parse-item [item]
  (->> (str/split item #"-")
       (mapv keyword)))

(defn build-nodes []
  (mapv parse-item lines))

(defn find-nodes-from [nodes [f t :as node]]
  (let [filtered (filter (fn [[a _]] (= a t)) nodes)]
    (if (and (empty? filtered)
             (u/uppercase? (str f)))
      [(vec (reverse node))]
      filtered)))

;(defn find-nodes-to [nodes node]
;  (filter (fn [[_ b]] (= node b)) nodes))

;(find-nodes-from nodes [:A :c])

(defn find-start [nodes] (find-nodes-from nodes [nil :start]))
;(defn find-end [nodes] (find-nodes-to nodes [:end]))

(defn multi-pass? [node]
  (->> (str node) (#(= (str/upper-case %) %))))

(def nodes (build-nodes))

(defn has-walked? [walked node]
  (contains? (into #{} walked) node))

(defn walk [nodes from walked]
  ;(loop [nodes nodes
  ;       from from
  ;       walked walked]
    (let [ns (find-nodes-from nodes from)
          filtered (remove (partial has-walked? walked) ns)
          n (first filtered)]
      (cond
        (not n) walked
        (= :end (second n)) (conj walked n)
        :else (recur nodes n (conj walked n)))))

(defn start [nodes]
  (walk nodes [nil :start] []))

(comment
  (filter multi-pass? nodes)
  (find-end nodes)
  (find-start nodes))