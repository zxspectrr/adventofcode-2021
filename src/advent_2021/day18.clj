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

(defn get-magnitude [number]
  (let [[a b] number
        va (if (number? a) a (get-magnitude a))
        vb (if (number? b) b (get-magnitude b))]
    (+ (* va 3) (* vb 2))))

(defn add-and-get-magnitude [numbers]
  (->> (add-numbers numbers)
       (get-magnitude)))

(defn part1 []
  (add-and-get-magnitude numbers))

(defn max-val [number1 number2]
  (->> (map add-and-get-magnitude [[number1 number2] [number2 number1]])
       (reduce max)))

(defn get-max-val-for-number [numbers number]
  (let [other-numbers (remove (partial = number) numbers)]
    (->> (map (partial max-val number) other-numbers)
         (reduce max))))

(defn get-max-val-for-numbers [numbers]
  (->> (map (partial get-max-val-for-number numbers) numbers)
       (reduce max)))

(defn part2 []
  (get-max-val-for-numbers numbers))


(comment
  (process [[6,[5,[4,[3,2]]]],1])
  (def root (zip/vector-zip [[6,[5,[4,[3,2]]]],1]))
  (def exp (find-explodable root))
  (def blank (zip/replace exp 0))
  ,)