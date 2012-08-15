(ns speling.autocorrect
  (:use [speling core]))
  
;; Implementation of the algorithm described by Peter Norvig
;; at http://norvig.com/spell-correct.html

(def ^{:private true} NWORDS (atom nil))

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


;; E.G. (swap-nwords (slurp "/usr/share/dict/words"))
(defmulti swap-nwords class)
(defmethod swap-nwords String [text]
  (swap-nwords
   (fn [old]
     (->> text
          (words)
          (train)))))
(defmethod swap-nwords clojure.lang.AFunction [f & args]
  (apply swap! NWORDS f args))

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
       (filter @NWORDS)
       (set)))

(defn known [words] (filter @NWORDS words))

(defn correct [word]
  (let [candidates (or-ne (known [word])
                          (known (edits1 word))
                          (known-edits2 word))]
    (->> candidates
         (select-keys @NWORDS)
         (sort-by last)
         (reverse)
         (first))))

(defn correct-phrase [s]
  (->> (words s)
       (map correct)
       (filter not-empty)
       (sort-by last)
       (reverse)))
