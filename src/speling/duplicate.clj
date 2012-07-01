(ns speling.duplicate
  (:use [speling core])
  (:require [clojure.java.jdbc :as sql]
            [clojure.set :as set]))

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

(defn names-map-sized [limit]
  (select-name-map ["SELECT id, name FROM mailers WHERE flags & 32 = 0 LIMIT ?" limit]))

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

(defn- delete-if [m pred]
  (select-keys m
   (for [[k v :as p] m :when (pred p)] k)))


(defn filtered-name-frequencies
  ([name-freqs] (filtered-name-frequencies name-freqs 1))
  ([name-freqs min-limit]
     (delete-if name-freqs (fn [[k v]] (> (count v) min-limit)))))

;; TODO: can this be made faster with transients?
(defn co-occurring-ids-map
  ([name-map] (co-occurring-ids-map name-map 3))
  ([name-map min-frequency]
     (let [groups (-> name-map
                      (name-frequencies)
                      (filtered-name-frequencies min-frequency)
                      (vals))
           ids (->> groups (apply concat) (distinct))]
       (loop [pairs groups
              gmap (zipmap ids (repeat {}))]
         (if (empty? pairs) gmap
             (recur (rest pairs)
                    (reduce
                     (fn [m p] (update-in m p (fn [v] (inc (or v 0)))))
                     gmap
                     (for [a (first pairs)
                           b (first pairs)
                           :when (not= a b)] [a b]))))))))

(defn best-matches
  ([name-map] (best-matches name-map 10))
  ([name-map n]
     (map
      (fn [[id matches]] [id (take n (reverse (sort-by last matches)))])
      (co-occurring-ids-map name-map 1))))

;; old version
(comment
  (apply merge-with
         (fn [list-a list-b] (merge-with + list-a list-b))
         imap
         (map (fn [id] {id (zipmap (disj ids id) (repeat 1)) }) ids)))

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

(defn -main []
  (do
    (for [c [100 1000 2500 5000 7500 10000 20000 35000 70000]]
      (time (println c "->" (count (doall (co-occurring-ids-map (names-map-sized c)))))))))
