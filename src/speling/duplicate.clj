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
              (ngrams name 3 7)))
    (transient {})
    name-map)))

(defn- delete-if [m pred]
  (select-keys m
   (for [[k v :as p] m :when (pred p)] k)))

(defn filtered-name-frequencies
  ([name-freqs] (filtered-name-frequencies name-freqs 1))
  ([name-freqs min-limit]
     (delete-if name-freqs (fn [[k v]] (> (count v) min-limit)))))

(defn ids-to-pairs-map [name-map min-frequency]
  (let [lfirst (comp last first)]
    (loop [pairs (-> name-map
                     (name-frequencies)
                     (filtered-name-frequencies min-frequency)
                     (seq))
           vmap (transient {})]
      (if (empty? pairs) (persistent! vmap)
          (let [ngram (ffirst pairs)
                v (count ngram)
                ids (lfirst pairs)]
            (recur (rest pairs)
                   (reduce
                    (fn [m [id other]]
                      (assoc! m id (conj (get m id []) [other v])))
                    vmap
                    (for [x ids y ids :when (not= x y)] [x y]))))))))

(defn nids-to-pairs-map [name-map min-frequency]
  (map
   
   (-> name-map
       (name-frequencies)
       (filtered-name-frequencies min-frequency)))
  )

(defn doit
  ([fnmap] (doit fnmap 100))
  ([fnmap maxn]
     (dotimes [_ 5]
       (time
        (let [idfn (comp (names-map) first)]
          (doall
           (pmap
            #(loop [names %]
               (if (empty? names)
                 'done
                 (do
                   (count
                    (map idfn
                         (reverse
                          (sort-by last
                                   (filter identity
                                           (map
                                            (fn [s] (if (> (count s) 3) [(first s) (count s)]))
                                            (partition-by identity
                                                          (sort
                                                           (apply concat
                                                                  (filter identity
                                                                          (map fnmap
                                                                               (ngrams (first names)))))))))))))
                   (recur (rest names)))))
            (partition-all (inc (.availableProcessors (Runtime/getRuntime)))
                           (map last (take maxn (names-map)))))))))))

(comment

  (def fnmap
    (-> (names-map)
        (name-frequencies)
        (filtered-name-frequencies 3))))

;; TODO: can this be made faster with transients?
(defn co-occurring-ids-map
  ([name-map] (co-occurring-ids-map name-map 3))
  ([name-map min-frequency]
     (let [idpm (ids-to-pairs-map name-map min-frequency)]
 (loop [ids (keys idpm)
        idpm (transient idpm)]
   (if (empty? ids) (persistent! idpm)
       (let [id (first ids)]
         (recur
          (rest ids)
          (assoc! idpm id
                  (pmap
                   (fn [gr] [ (ffirst gr) (reduce + (map last gr))])
                   (->> (get idpm id '())
                        (sort-by first)
                        (partition-by first))))
          )))))))

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

(defn -main []
  (let [fnmap (-> (names-map)
                  (name-frequencies)
                  (filtered-name-frequencies 3)
                  (doall))
        _ (println "Starting test...")]
    (doit fnmap 1000000)))

(comment
  (for [c (range 100 60100 5000)]
    (do
      (time
       (println c "=>"
        (-> c
            (names-map-sized)
            (co-occurring-ids-map)
            (doall)
            (count))))
      c)))