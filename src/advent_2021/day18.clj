(ns advent-2021.day18
  (:require [advent-2021.utils :as u]
            [clojure.zip :as zip]))

(def numbers
  (->> (u/read-lines "resources/day18/input.txt")
       (map read-string)))

;(def numbers [[1,1] [2,2] [3,3] [4,4] [5,5]])

(defn find-explodable-node [root]
  (loop [[node-value _node-info :as node] root]
    (cond
      (zip/end? node) nil
      (and (zip/branch? node)
           (number? (first node-value))
           (number? (second node-value))) node
      :else (recur (zip/next node)))))

(defn find-left-num [node]
  (loop [node node
         path-count 0]
    (cond
      (nil? node) [node path-count]
      (zip/branch? node) (recur (zip/prev node) (inc path-count))
      :else [node path-count])))

(defn update-left [node]
  (let [[ln pc] (find-left-num node)]
    (if ln
      (let [[[a _b] _] node
            updated (zip/edit ln + a)]
        (->> (iterate zip/next updated)
             (take (inc pc))
             (last)))
      node)))

(defn find-right-num [node]
  (let [outer (->> (zip/next node) zip/next zip/next)]
    (loop [n outer
           path-count 3]
      (cond
        (zip/branch? n) (recur (zip/next n) (inc path-count))
        (zip/end? n) [nil path-count]
        :else [n path-count]))))

(defn update-right [node]
  (let [[rn pc] (find-right-num node)]
    (if rn
      (let [[[_a b] _] node
            updated (zip/edit rn + b)]
        (->> (iterate zip/prev updated)
             (take (inc pc))
             (last)))
      node)))

(defn explode [node]
  (-> (update-left node)
      (update-right)
      (zip/replace 0)
      (zip/root)))

(comment

  (zip/next root)

  (def root (zip/vector-zip [[3 [2 [ 1,[ 7,3 ]]]] [ 6,[ 5,[ 4,[ 3,2]]]]]))

  (-> (zip/vector-zip [[[[[9,8],1],2],3],4])
      (find-explodable-node)
      (explode))


  ,)



