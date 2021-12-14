(ns advent-2021.utils
  (:require [clojure.string :as str]))

(defn map-vals [f m] (into {} (map (fn [[k v]] [k (f v)]) m)))
(defn pivot [dataset] (apply map vector dataset))
(defn parse-int [str] (Integer/parseInt str))
(defn read-lines [path]
  (->> (slurp path)
       (str/split-lines)))
