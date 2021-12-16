(ns advent-2021.utils
  (:require [clojure.string :as str]))

(defn map-vals [f m] (into {} (map (fn [[k v]] [k (f v)]) m)))
(defn map-keys [f m] (into {} (map (fn [[k v]] [(f k) v]) m)))
(defn pivot [dataset] (apply map vector dataset))
(defn parse-int [str] (Integer/parseInt str))
(defn uppercase? [s] (= s (str/upper-case s)))
(defn read-lines [path]
  (->> (slurp path)
       (str/split-lines)))
