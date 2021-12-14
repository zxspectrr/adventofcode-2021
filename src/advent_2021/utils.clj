(ns advent-2021.utils)

(defn map-vals [f m] (into {} (map (fn [[k v]] [k (f v)]) m)))
(defn pivot [dataset] (apply map vector dataset))
(defn parse-int [str] (Integer/parseInt str))
