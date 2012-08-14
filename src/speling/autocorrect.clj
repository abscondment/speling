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


(comment (def NWORDS (train (words (slurp "small.txt"))))
         (def NWORDS (train (words (slurp "big.txt"))))
         (def NWORDS (train (words (slurp "/usr/share/dict/words")))))


;; TODO: supply a custom list of words
(def NWORDS
  (->> (slurp "/usr/share/dict/words")
       (mapcat words)
       (filter not-empty)
       (train)))


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
    (or (->> candidates
             (select-keys NWORDS)
             (sort-by last)
             (reverse)
             (ffirst))
        word)))

(defn correct-phrase [s]
  (map correct (words s)))