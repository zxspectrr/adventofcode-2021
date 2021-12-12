(ns advent-2021.day10
  (:require [clojure.string :as str]))

(def lines
  (->> (slurp "resources/day10.txt")
       (str/split-lines)
       (map vec)))

(def open-chars #{\[ \( \{ \<})
(def close-char-map {\[ \] \( \) \{ \} \< \>})
(def corrupt-char-scores {\] 57 \) 3 \} 1197 \> 25137})
(def autocomplete-char-scores {\) 1 \] 2 \} 3 \> 4})

(defn open-char? [char] (some? (open-chars char)))
(defn find-closing-char [open-char] (close-char-map open-char))

(defn correct-close-char? [char current-open-char]
  (= (find-closing-char current-open-char) char))

(defn valid-char? [char current-open-char]
  (or (nil? current-open-char)
      (open-char? char)
      (correct-close-char? char current-open-char)))

(defn safe-pop [stack]
  (if (not-empty stack) (pop stack) stack))

(defn walk-line [line]
  (letfn [(next-stack [stack char]
            (if (open-char? char)
              (conj stack char)
              (safe-pop stack)))]

    (loop [chars line
           char-stack (list)]
      (let [current-char (first chars)]
        (cond
          (nil? current-char) {:stack char-stack}
          (not (valid-char? current-char (peek char-stack))) {:bad-char current-char}
          :else (recur (rest chars)
                       (next-stack char-stack current-char)))))))

(defn auto-complete-score-for-chars [{:keys [stack]}]
  (->> (map close-char-map stack)
       (reduce (fn [total-score char]
                 (->> (* total-score 5)
                      (+ (autocomplete-char-scores char))))
               0)))

(defn middle-value [coll]
  (nth coll (quot (count coll) 2)))

(defn part1 []
  (->> (map walk-line lines)
       (keep :bad-char)
       (map corrupt-char-scores)
       (apply +)))

(defn part2 []
  (->> (map walk-line lines)
       (remove :bad-char)
       (map auto-complete-score-for-chars)
       (sort)
       (middle-value)))