(ns advent-2021.day18
  (:require [advent-2021.utils :as u]
            [clojure.zip :as zip]))

(def numbers
  (->> (u/read-lines "resources/day18/input.txt")
       (map read-string)))

(defn edge? [node]
  (or (nil? node) (zip/end? node)))

(def depth (comp count zip/path))

(defn explodable? [node]
  (and (= (depth node) 4)
       (zip/branch? node)))

(defn find-node [node pred direction]
  (loop [node (direction node)]
    (cond
      (edge? node) nil
      (pred node) node
      :else (recur (direction node)))))

(def leaf? (complement zip/branch?))
(defn next-leaf? [node] (find-node node leaf? zip/next))
(defn prev-leaf? [node] (find-node node leaf? zip/prev))

(defn do-explode [node]
  (letfn [(increment-left [node v]
            (if-let [leaf (prev-leaf? node)]
              (next-leaf? (zip/edit leaf + v))
              node))

          (increment-right [node v]
            (if-let [leaf (next-leaf? node)]
              (prev-leaf? (zip/edit leaf + v))
              node))]

    (let [[a b] (zip/node node)]
      (-> (zip/replace node 0)
          (increment-left a)
          (increment-right b)
          (zip/root)))))

(defn find-explodable [root]
  (when root
    (find-node root explodable? zip/next)))

(defn explode [number]
  (let [root (zip/vector-zip number)]
    (if-let [exp (find-explodable root)]
      (do-explode exp)
      number)))

(defn process [number]
  (reduce (fn [acc x]
            (if (= acc x) (reduced x) x))
          (iterate explode number)))

(comment
  (process [[6,[5,[4,[3,2]]]],1])
  (def root (zip/vector-zip [[6,[5,[4,[3,2]]]],1]))
  (def exp (find-explodable root))
  (def blank (zip/replace exp 0))
  ,)