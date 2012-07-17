(ns speling.autocorrect
  (:use [speling core])
  (:require [speling.db :as db]
            [clojure.string :as string]))
  
;; Implementation of the algorithm described by Peter Norvig
;; at http://norvig.com/spell-correct.html

;; TODO:
;;  * get words from the database
;;  * training should *allow* for some DB-specific weight
;;    to be applied at the word level

(defn train
  ([features] (train features 1))
  ([features weight] (train features weight {}))
  ([features weight model]
     (loop [features features
            model (transient model)]
       (if (empty? features)
         (persistent! model)
         (recur (rest features)
                (assoc! model
                        (first features)
                        (+ weight (get model (first features) 1))))))))

(defn train-map [maps]
  (reduce
   (fn [model {count :count words :words}]
     (train words count model))
   {} maps))

(comment (def NWORDS (train (words (slurp "small.txt"))))
         (def NWORDS (train (words (slurp "big.txt"))))
         (def NWORDS (train (words (slurp "/usr/share/dict/words")))))

(def NWORDS
  (->> (db/active-names-counts-map)
       (map last)
       (map
        (fn [m]
          (let [n (string/join " " (map (partial get m) [:name :url :email]))
                c (get m :count)]
            (if (not-empty n)
              (assoc m
                :count (if (zero? c) 1.0 (Math/log c))
                :words ((comp remove-stopwords words) n))))))
       (filter identity)
       (train-map)))

(def alphabet (seq "abcdefghijklmnopqrstuvwxyz1234567890"))

(defn edits1 [word]
  (let [splits (map #(split-at % word) (range (count word)))
        deletes (for [[a b] splits] (concat a (next b)))
        transposes (for [[a [b c & d]] splits :when d] (concat a [c b] d))
        replaces (for [[a b] splits c alphabet] (concat a [c] (next b)))
        inserts (for [[a b] splits c alphabet] (concat a [c] b))]
    (->> (concat deletes transposes replaces inserts)
         (map (partial apply str))
         (set))))

(defn known-edits2 [word]
  (->> (edits1 word)
       (mapcat edits1)
       (filter NWORDS)
       (set)))

(defn known [words] (filter NWORDS words))

(defn correct [word]
  (let [candidates (or-ne (known [word])
                          (known (edits1 word))
                          (known-edits2 word))]
    (->> candidates
         (select-keys NWORDS)
         (sort-by last)
         (reverse)
         (first))))

(defn correct-phrase [s]
  (->> (words s)
       (map correct)
       (filter not-empty)
       (sort-by last)
       (reverse)))