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

(defn compare-names
  ([nmap f] (compare-names nmap f {}))
  ([nmap f opts]
     (let [count-filter-level (get opts :count-filter-level 3)
           freq-filter-level  (get opts :freq-filter-level 3)
           idfn (comp nmap first)
           fnmap (-> nmap
                     (name-frequencies)
                     (filtered-name-frequencies freq-filter-level))]
       (doall
        (pmap
         #(doseq [[id name] %]
            ;;
            ;; TODO: weigh each match by the ngram length
            ;;
            (let [matches (->> (ngrams name)
                               (map fnmap)
                               (filter identity)
                               (apply concat)
                               (sort)
                               (partition-by identity)
                               (map (fn [s] (if (> (count s) count-filter-level) [(first s) (count s)])))
                               (filter identity)
                               (sort-by last)
                               (reverse))]
              (f id name matches)))
         (partition-all (/ (count nmap) (inc (.availableProcessors (Runtime/getRuntime)))) nmap))))))

(comment
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
        (co-occurring-ids-map name-map 1)))))

(defn -main []
  (do
    (time
     (compare-names (fn [id name matches]
                      (do (spit (str "output/" id ".match") (-> matches (vec) (str)))))
                    (names-map)))
    (shutdown-agents)))