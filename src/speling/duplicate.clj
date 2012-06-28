(ns speling.duplicate
  (:use [speling core])
  (:require [clojure.java.jdbc :as sql]))

(def db {:subprotocol "mysql"
         :subname "//127.0.0.1:3306/paperkarma_development"
         :user "root"
         :password nil})

(defn- select-name-map [query-coll]
  (sql/with-connection db
    (sql/with-query-results rows
      query-coll
      (reduce
       (fn [m row] (assoc m (:id row) (:name row)))
       {} rows))))

(defn new-names-map []
  (select-name-map ["SELECT m.id, m.name FROM (SELECT mailer_id FROM unsubscribes GROUP BY mailer_id HAVING count(*) < 3 AND count(*) > 0) u INNER JOIN mailers m ON m.id = u.mailer_id AND flags & 32 = 0"]))

(defn names-map []
  (select-name-map ["SELECT id, name FROM mailers WHERE flags & 32 = 0"]))

(defn names-small-map []
  (select-name-map ["SELECT id, name FROM mailers WHERE flags & 32 = 0 LIMIT 1000"]))

(defn ngrams
  ([text] (ngrams text 5))
  ([text max-n] (ngrams text 2 max-n))
  ([text min-n max-n]
     (mapcat
      #(apply concat
              (for [n (range min-n (inc max-n))]
                (partition n 1 %)))
      (words text))))

(defn name-frequencies [name-map]
  (persistent!
   (reduce
    (fn [fmap [id name]]
      (reduce (fn [m n] (assoc! m n (conj (get m n #{}) id)))
              fmap
              (ngrams name)))
    (transient {})
    name-map)))

;; TODO: can this be made faster with transients?
(defn co-occurring-ids-map [name-map]
  (reduce
   (fn [imap [ngram ids]]
     (apply merge-with
            (fn [list-a list-b] (merge-with + list-a list-b))
            imap
            (map (fn [id] {id (zipmap (disj ids id) (repeat 1)) }) ids)))
   {}
   (name-frequencies name-map)))

(comment
 (time (pprint
        (partition 2
                   (apply concat
                          (take 20
                                (sort-by (comp count last)
                                         (filter #(> (-> % (last) (count)) 4)
                                                 (name-frequencies (names-map))))))))))

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