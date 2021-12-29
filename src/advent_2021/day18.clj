(ns advent-2021.day18
  (:require [advent-2021.utils :as u]
            [clojure.zip :as zip]))

(def numbers
  (->> (u/read-lines "resources/day18/input.txt")
       (map read-string)))

(defn find-explodable-node [root]
  (loop [[node-value node-info :as node] root]
    (let [parent-count (count (:pnodes node-info))]
      (cond
        (zip/end? node) nil
        (and (zip/branch? node)
             (> parent-count 3)
             (number? (first node-value))
             (number? (second node-value))) node
        :else (recur (zip/next node))))))

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
        (zip/end? n) [nil path-count]
        (zip/branch? n) (recur (zip/next n) (inc path-count))
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

(defn process-number [number]
  (let [root (zip/vector-zip number)
        explodable (find-explodable-node root)]
    (if explodable
      (explode explodable)
      number)))

(comment

  ,)



