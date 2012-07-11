(ns speling.autocorrect
  (:use [speling core]))

;; Implementation of the algorithm described by Peter Norvig
;; at http://norvig.com/spell-correct.html

;; TODO:
;;  * get words from the database
;;  * training should *allow* for some DB-specific weight
;;    to be applied at the word level

(defn train [features]
  (loop [features features
         model (transient {})]
    (if (empty? features)
      (persistent! model)
      (recur (rest features)
             (assoc! model
                     (first features)
                     (inc (get model (first features) 1)))))))


(comment (def NWORDS (train (words (slurp "small.txt")))))
(comment (def NWORDS (train (words (slurp "big.txt")))))
(def NWORDS (train (words (slurp "/usr/share/dict/words"))))

(def alphabet (seq "abcdefghijklmnopqrstuvwxyz"))

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
         (ffirst))))