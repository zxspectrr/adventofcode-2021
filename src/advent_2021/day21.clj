(ns advent-2021.day21)

(def start-pos [1 2])

(defn roll
  ([] 1)
  ([previous-value]
   (if (> previous-value 99 ) 1 (inc previous-value))))

(def board (cycle (range 1 11)))

(defn move [position steps]
  (nth board (+ position (dec steps))))


(def game-state {:position 1})

(comment
  (move 7 8)

  (nth (cycle (range 1 11)) 0)
  (take 2 (take 1 dice))
  (take 2 (take 1 next-dice))


 ,)