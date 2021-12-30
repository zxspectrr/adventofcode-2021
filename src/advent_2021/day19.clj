(ns advent-2021.day19
  (:require [clojure.set :as set]))

(slurp "resources/day19/input.txt")

(def pos [1,2,3])

(def flip (partial * -1))

(defn rotate-y [[x y z]]
  [z y (flip x)])

(defn rotate-x [[x y z]]
  [x z (flip y)])

(defn rotate-z [[x y z]]
  [(flip y) x z])

(defn rotate-combinations [pos rotate-fn]
  (take 4 (iterate rotate-fn pos)))

(defn x-rotate-combinations [pos]
  (rotate-combinations pos rotate-x))

(defn y-rotate-combinations [pos]
  (rotate-combinations pos rotate-y))

(defn z-rotate-combinations [pos]
  (rotate-combinations pos rotate-z))

(comment

  (mapcat (partial rotate-combinations pos) [rotate-x rotate-y rotate-z])

  (take 4 (iterate rotate-left pos))
  (rotate-left [-1 1 2]))