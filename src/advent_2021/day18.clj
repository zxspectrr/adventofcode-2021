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

(defn find-explodable [root]
  (find-node root explodable? zip/next))

(def leaf? (complement zip/branch?))

(defn next-leaf? [node] (find-node node leaf? zip/next))
(defn prev-leaf? [node] (find-node node leaf? zip/prev))

(defn do-explode [node]
  (->> (zip/replace node 0)))

(defn explode [root]
  (if-let [exp (find-explodable root)]
    (->> (reduce (fn [acc x]
                   (if (= acc x) (reduced x) x))
                 (iterate do-explode exp))
         (zip/root))
    root))

(defn process [number]
  (->> (zip/vector-zip number)
       (explode)))

(comment

  (depth exp)

  (def root (zip/vector-zip [[[[[9 8],1 ],2 ],3 ],4]))
  (def exp (find-explodable root))
  (def blank (zip/replace exp 0))

  (process [[[[[9 8],1 ],2 ],3 ],4])

  ,)


                
