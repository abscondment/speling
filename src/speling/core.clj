(ns speling.core
  (:require [clojure.string :as str]))

(defn words [text]
  (->
   text
   (str/lower-case)
   (str/replace #"[']+" "'")
   (str/replace #"[\-]+" "-")
   (str/split #"[^a-z0-9'\-]+([^a-z0-9][^a-z0-9'\-]?)*")))