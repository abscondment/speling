(ns speling.core
  (:use [clojure.string :only [split]]))

(defn words [text] (clojure.string/split (.toLowerCase text) #"[^a-z0-9]+"))

