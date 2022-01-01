(ns advent-2021.scratch)

(defn +* [& xs]
  (reduce (fn [[a b] [c d]] [(+ a c) (+ b d)]) [0 0] xs))
  ;(apply mapv + xs))

(+* [1 2 3] [2 3 4] [4 5 6])

(mapv + [1 2] [3 4])

(mapv #(* 2 %) [1 2 3])