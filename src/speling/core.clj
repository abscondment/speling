(ns speling.core
  (:use [clojure.string :only [split]])
  (:require [clojure.java.jdbc :as sql]))

(defn words [text] (clojure.string/split (.toLowerCase text) #"[^a-z0-9]+"))

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

(defn ngrams
  ([text] (ngrams text 5))
  ([text max-n] (ngrams text 2 max-n))
  ([text min-n max-n]
     (mapcat
      #(apply concat
              (for [n (range min-n (inc max-n))]
                (partition n 1 %)))
      (words text))))

(defn make-freqs [coll]
  (reduce
   (fn [counts word]
     (let [c (get counts word 0)]
       (assoc counts word (inc c))))
   {} coll))

(defn freqs-for
  ([text] (freqs-for text 150))
  ([text use-n-freqs]
     (->> text
          (#(ngrams % 3 7))
          (make-freqs)
          (sort-by #(* -1 (last %) (count (first %))))
          (map first)       
          (take use-n-freqs))))

(comment
 (defn ngramdiff
   ([a b] (ngramdiff a b 150))
   ([a b use-n-freqs]
      (let [freqmap-fn #(apply hash-map (interleave % (iterate inc 0)))
            freqs-a (freqs-for a)
            freqs-b (freqs-for b)

            max-d (* (max (count freqs-a) (count freqs-b))
                     (Math/abs (- (count freqs-a)
                                  (count freqs-b))))

            freqmap-a (freqmap-fn freqs-a)
            freqmap-b (freqmap-fn freqs-b)]
        (quot
         (reduce +
                 (for [ngram (concat freqs-a freqs-b)]
                   (let [ad (get freqmap-a ngram max-d)
                         bd (get freqmap-b ngram max-d)]
                     (Math/abs (- ad bd)))))
         (quot (+ (count freqs-a) (count freqs-b)) 2))))))

(def db {:subprotocol "mysql"
         :subname "//127.0.0.1:3306/paperkarma_development"
         :user "root"
         :password nil})

(def new-names
  (sql/with-connection db
    (sql/with-query-results rows
      ["SELECT m.name FROM (SELECT mailer_id FROM unsubscribes GROUP BY mailer_id HAVING count(*) < 3 AND count(*) > 0) u INNER JOIN mailers m ON m.id = u.mailer_id AND flags & 32 = 0"]
      (doall (map :name rows)))))

(def names
  (sql/with-connection db
    (sql/with-query-results rows
      ["SELECT name FROM mailers WHERE flags & 32 = 0"]
      (doall (map :name rows)))))

(comment
  (let [all-ngrams
        (map names
             ()
             )
        ])

  (time
   (pprint
    (pmap #(list
            %
            (take 100
                  (rest
                   (sort-by last
                            (map (fn [n] [n (ngramdiff % n 150)]) names)))))
          (take 5 (drop 2000 new-names)))))
  )