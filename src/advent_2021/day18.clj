(ns advent-2021.day18
  (:require [advent-2021.utils :as u]
            [clojure.zip :as zip]))

(def numbers
  (->> (u/read-lines "resources/day18/input.txt")
       (map read-string)))

(defn edge? [node]
  (or (nil? node) (zip/end? node)))

(def depth (comp count zip/path))
(def leaf? (complement zip/branch?))

(defn explodable? [node]
  (and (= (depth node) 4)
       (zip/branch? node)))

(defn splittable? [node]
  (and (leaf? node) (> (zip/node node) 9)))

(defn find-node [node pred direction]
  (loop [node (direction node)]
    (cond
      (edge? node) nil
      (pred node) node
      :else (recur (direction node)))))

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

(defn do-split [node]
  (let [val (zip/node node)]
    (-> (zip/replace node [(int (Math/floor (/ val 2)))
                           (int (Math/ceil (/ val 2)))])
        (zip/root))))

(defn adjust-number [number]
  (condp (fn [expr dataset]
           (find-node dataset expr zip/next))
         (zip/vector-zip number)

    explodable? :>> do-explode
    splittable? :>> do-split
    number))

(defn reduce-number [number]
  (reduce (fn [acc x]
            (if (= acc x) (reduced x) x))
          (iterate adjust-number number)))

(defn add-numbers [numbers]
  (reduce (fn [acc x]
            (reduce-number
              (if acc [acc x] x)))
          nil
          numbers))

(comment

  (add-numbers [[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
                [7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
                [[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
                [[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
                [7,[5,[[3,8],[1,4]]]]
                [[2,[2,2]],[8,[8,1]]]
                [2,9]
                [1,[[[9,3],9],[[9,0],[0,7]]]]
                [[[5,[7,4]],7],1]
                [[[[4,2],2],6],[8,7]]])

  (process [[[[0,7],4],[15,[0,13]]],[1,1]])

  (process [[6,[5,[4,[3,2]]]],1])
  (def root (zip/vector-zip [[6,[5,[4,[3,2]]]],1]))
  (def exp (find-explodable root))
  (def blank (zip/replace exp 0))
  ,)