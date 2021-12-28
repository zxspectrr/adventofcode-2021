(ns advent-2021.day18
  (:require [advent-2021.utils :as u]))

(def numbers
  (->> (u/read-lines "resources/day18/input.txt")
       (map read-string)))

(def numbers [[1,1] [2,2] [3,3] [4,4] [5,5]])

(defn add-numbers [numbers]
  (reduce (fn [acc x]
            (if acc [acc x] x)) nil numbers))

(def exploding-number [7 [6 [5 [4 [3 2]]]]])

[7 [6 [5 [4 [3 2]]]]]

(def c {:cell [3 2] :left [7 [6 [5 [4 [3 2]]]]] :right []})

(number? 1)

(defn literal-pair? [[first last]]
  (and (number? first) (number? last)))

(defn count-parents [arr total]
  (reduce (fn [acc x]
            (if (number? x)
              total
              (count-parents x (inc acc))))
          total
          arr))

(comment
  (count-parents [7 [6 [5 [4 [3 2]]]]] 0)

  (count [4 [5 4] [6 5 4] [7 6 5 4]])

  [[3 2], [4 [5 4] [6 5 4] [7 6 5 4]]]

  [[3 2], [[7 [6 [5 [4 [3 2]]]]],
           [6 [5 [4 [3 2]]]],
           [5 [4 [3 2]]],
           [4 [3 2]]]]

  ,)

(rest (conj (list [1] [2] [3]) [4]))

(defn walk [arr parents]

  (reduce (fn [acc x]
            (if (number? x)
              x
              (conj acc
                    (walk x (conj parents  arr)))))
          {}
          arr))

(declare walk2)

(defn find-first-a [parents]
  (reduce (fn [acc [a _]]
            (if (and (number? a) (not acc))
              a
              acc))
          nil
          parents))

(defn adjust [v parents]
  (let [[a b] v
        explode? (> (count parents) 3)]

    (cond (number? v) v
          explode? (find-first-a)

      (walk2 v parents))))

(defn walk2 [arr parents]
  (let [[a b] arr
        explode? (> (count parents) 3)]
    ;(when explode?
    (prn parents)
    (let [new-parents (cons arr parents)
          new-a (adjust a new-parents)
          new-b (adjust b new-parents)]
      [new-a new-b])))



(comment
  (walk2 [7 [6 [5 [4 [3 2]]]]] [])


  (let [[a b] [7 [6 [5 [4 [3 2]]]]]]
    (prn a)
    (prn b)
    [a b])

  (walk [7 [6 [5 [4 [3 2]]]]] nil)
  ,)




(count [7 [6 [5 [4]]]])

[7 [6 [5 [7 0]]]]


[[5 5] [6 7]]

(defn explode? [parent-count] (> parent-count 3))

[[9 8]   [ [ [ [ [9 8] 1] 2 ] 3] 4]]


(explode? 3)


(coll? 1)

(comment
  (add-numbers numbers)
  ,)