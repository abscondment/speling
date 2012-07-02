(ns speling.duplicate
  (:use [speling core]
        [clojure pprint])
  (:require [clojure.java.jdbc :as sql]
            [clojure.set :as set]))

(def db {:subprotocol "mysql"
         :subname "//33.33.33.33:3306/paperkarma_development"
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
                      (filtered-name-frequencies min-frequency))
           ids (->> groups (vals) (apply concat) (distinct))]
       (loop [pairs (seq groups)
              gmap (zipmap ids (repeat {}))]
         (do
           (if (empty? pairs) gmap
               (let [[[ngram matched-ids] & more-pairs] pairs]
                (recur more-pairs
                       (reduce
                        ;; TODO: weight ngrams by size
                        (fn [m p] (update-in m p (fn [v] (+ (count ngram) (or v 0)))))
                        gmap
                        (for [a matched-ids
                              b matched-ids
                              :when (not= a b)] [a b]))))))))))

(defn best-matches
  ([name-map] (best-matches name-map 10 (/ Math/PI 2.0)))
  ([name-map n-matches min-weight]
     (map
      (fn [[id matches]]
        [(name-map id)
         (take n-matches
          (reverse
           (sort-by last
            (filter (fn [[k v]] (> v min-weight))
                    (map (fn [[k v]] [(name-map k) (/ v (count (name-map k)))])
                         matches)))))])
      (co-occurring-ids-map name-map 1))))

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

(defn -main []
  (do
    (for [c [100 1000 2500 5000 7500 10000 20000 35000 70000]]
      (time (let [cm (doall (best-matches (names-map-sized c)))]
              (do
                (println c "->" (count cm))
                (pprint (take 5 cm))
                (println  "...")))))))
