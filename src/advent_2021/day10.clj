(ns advent-2021.day10
  (:require [clojure.string :as str]))

(def lines
  (->> (slurp "resources/day10.txt")
       (str/split-lines)
       (map vec)))

(def open-chars #{\[ \( \{ \<})
(def close-chars #{\] \) \} \>})
(def close-map {\[ \] \( \) \{ \} \< \>})
(def open-map {\] \[ \) \( \} \{ \> \<})

(defn open-char? [char] (some? (open-chars char)))
(defn find-closing-char [open-char] (close-map open-char))

(defn correct-close-char? [char current-open-char]
  (= (find-closing-char current-open-char)
     char))

(defn valid-char? [char current-open-char]
  (or (nil? current-open-char)
      (open-char? char)
      (correct-close-char? char current-open-char)))

(defn find-corrupt-char [line]
  (letfn [(next-stack [stack char]
            (if (open-char? char)
              (conj stack char)
              (pop stack)))]

    (loop [chars line
           open-stack (list)]
      (let [current-char (first chars)
            current-open-char (peek open-stack)]
        (cond
          (nil? current-char) nil
          (not (valid-char? current-char current-open-char)) current-char
          :else (recur (rest chars) (next-stack open-stack)))))))

(def corrupt-char-scores
  {\] 57 \) 3 \} 1197 \> 25137})

(defn part1 []
  (->> (keep find-corrupt-char lines)
       (map corrupt-char-scores)
       (apply +)))
