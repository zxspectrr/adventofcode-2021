(ns advent-2021.day18
  (:require [advent-2021.utils :as u]
            [clojure.zip :as zip]))

(def numbers
  (->> (u/read-lines "resources/day18/input.txt")
       (map read-string)))
;
;(def numbers [[1,1] [2,2] [3,3] [4,4] [5,5]])
;
;
;(defn go-in [zipper]
;  (zip/right (zip/down zipper)))

;(defn check-nodes [zipper]
;  (let [a (zip/node zipper)
;        b (zip/node (zip/right zipper))]
;    (number) [a b]))

(defn leaf? [[n _]]
  (when (coll? n)
    (let [[a b] n]
      (and (number? a) (number? b)))))

(def branch? (complement leaf?))

(defn explode? [[n node-info]]
  (let [{:keys [pnodes]} node-info
        parent-count (count pnodes)]
    (> parent-count 3)))

(defn find-leaf [vec]
  (->> (zip/vector-zip vec)
       (iterate zip/next)
       (take-while branch?)
       (last)
       (zip/next)))

(comment

  (-> (zip/vector-zip [1 2 3])
      zip/down
      zip/right
      (zip/replace [5 6])
      zip/root)


  (->> (find-leaf [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
       (explode?))

  (->> (zip/vector-zip [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
       (iterate zip/next)
       (take 100)
       (last)
       (zip/node))

  ,)


