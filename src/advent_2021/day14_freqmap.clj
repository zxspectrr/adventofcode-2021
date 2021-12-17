(ns advent-2021.day14-freqmap
  (:require [advent-2021.utils :as u]
            [clojure.string :as str]
            [clojure.set :as set]))
;
;(def lines (u/read-lines "resources/day14/small.txt"))
;
;(defn get-template []
;  (->> (apply vector (first lines))))
;       ;(map str)))
;
;(def rule-map
;  (->> (split-with #(not (= "" %)) lines)
;       (second)
;       (rest)
;       (map #(str/split % #" -> "))
;       (map (fn [[a b]] {(apply vector a) (first (apply vector b))}))
;       (apply merge-with into)))


(defn split-blank-line
  "Given an input string, returns a sequence of sub-strings, separated by a completely
  blank string. This function preserves any newlines between blank lines, and it filters
  out Windows' \"\r\" characters."
  [input]
  (-> (str/replace input "\r" "")
      (str/split #"\n\n")))

(defn load-data [] (slurp "resources/day14/small.txt"))

(defn parse-input []
  (let [[template rules] (split-blank-line (load-data))]
    [template
     (->> (re-seq #"\w+" rules) (partition 2) (map vec) (into {}))]))


(def template-map (second (parse-input)))
(def template (first (parse-input)))


(defn update-add [m k n] (update m k #(+ (or % 0) n)))

(defn apply-rules [rules freqs]
  (reduce (fn [acc [[a b :as word] n]] (let [c (rules word)]
                                         (-> acc
                                             (update-add (str a c) n)
                                             (update-add (str c b) n))))
          {} freqs))

(defn score [initial-template freqs]
  (println freqs)
  (let [char-freqs (reduce (fn [acc [[a] n]]
                             (println acc a n)
                             (update-add acc a n))
                           {(last initial-template) 1}
                           freqs)]
    char-freqs))
;    sorted-instances (sort-by - (vals char-freqs))]
;(apply - ((juxt first last) sorted-instances))))

(defn solve [step]
  (let [initial-freqs (->> template
                           (partition 2 1)
                           (map (partial apply str))
                           frequencies)]
    (->> (iterate (partial apply-rules template-map) initial-freqs)
         (drop step)
         first
         (score template))))

(solve 40)

(comment

  (defn part1 []
    (->> (take 11 (iterate step (get-template)))
         (last)
         (score)))

  (defn part2 []
    (->> (take 41 (iterate step (get-template)))
         (last)
         (score))))



