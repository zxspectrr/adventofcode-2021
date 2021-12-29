(ns advent-2021.day18
  (:require [advent-2021.utils :as u]
            [clojure.zip :as zip]))

(def numbers
  (->> (u/read-lines "resources/day18/input.txt")
       (map read-string)))

(defn edge? [node]
  (or (nil? node) (zip/end? node)))

(defn explodable? [node]
  (let [parent-count (count (zip/path node))
        n (zip/node node)]
    (and (zip/branch? node)
         (> parent-count 3)
         (number? (first n))
         (number? (second n)))))

(defn find-node [node pred direction]
  (loop [node node]
    (cond
      (edge? node) nil
      (pred node) node
      :else (recur (direction node)))))

(comment

  (find-node root explodable? zip/prev)

  (def root (zip/vector-zip [[[[[9,8],1],2],3],4]))

  (def iter (iterate zip/next root))

  (def child (last (take 4 iter)))

  (explodable? (zip/next child))

  (zip/next child)

  (zip/branch? child)

  (count (zip/path child))

  (zip/node child)

  (zip/path (last (take 4 iter)))

  (def beginning (zip/prev root))
  (def end (last (take 100 (iterate zip/next root))))



  (edge? beginning)
  (edge? end)


  true
  ,)
