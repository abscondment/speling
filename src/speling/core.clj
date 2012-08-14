(ns speling.core
  (:require [clojure.string :as str]))

(defn real-pmap [f s] (map deref (doall (map #(future (f %)) s))))

(defn delete-if [m pred]
  (select-keys m
   (for [[k v :as p] m :when (pred p)] k)))

(defn words [text]
  (-> text
      (str/lower-case)
      (str/replace #"[']+" "'")
      (str/replace #"[\-]+" "-")
      (str/split #"[^a-z0-9'\-]+([^a-z0-9][^a-z0-9'\-]?)*")
      (#(filter not-empty %))))

(def STOPWORDS
  (set (str/split "a an at as by com do http in of net org the with www" #"[\s]+")))

(defn remove-stopwords [coll]
  (for [w coll :when (not (contains? STOPWORDS w))] w))

(defn ngrams
  ([text] (ngrams text 5))
  ([text max-n] (ngrams text 2 max-n))
  ([text min-n max-n]
     (mapcat
      #(apply concat
              (for [n (range min-n (inc max-n))]
                (partition n 1 %)))
      (words text))))

(defmacro or-ne
  "Like or, but treats empty forms as if they have a logical false value."
  ([] nil)
  ([x] x)
  ([x & next]
     `(let [or# ~x]
        (if (and or# (not-empty or#)) or# (or-ne ~@next)))))
