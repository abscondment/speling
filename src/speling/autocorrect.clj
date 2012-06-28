(ns speling.autocorrect)

(defn train [features]
  (loop [features features
         model (transient {})]
    (if (empty? features)
      (persistent! model)
      (recur (rest features)
             (assoc! model
                     (first features)
                     (inc (get model (first features) 1)))))))


                                        ;(def NWORDS (train (words (slurp "big.txt"))))
(def NWORDS (train (words (slurp "small.txt"))))
(def alphabet (seq "abcdefghijklmnopqrstuvwxyz"))

(defn edits [word]
  (let [splits (map #(split-at % word) (range (count word)))
        deletes (for [[a b] splits] (concat a (next b)))
        transposes (for [[a [b c & d]] splits :when d] (concat a [c b] d))
        replaces (for [[a b] splits c alphabet] (concat a [c] (next b)))
        inserts (for [[a b] splits c alphabet] (concat a [c] b))]
    (apply hash-set
           (map (partial apply str) (concat deletes transposes replaces inserts)))))

(defn known-edits [word]
  (apply hash-set
         ))