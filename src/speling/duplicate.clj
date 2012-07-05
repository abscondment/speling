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

(defn- new-names-map []
  (select-name-map ["SELECT m.id, m.name FROM (SELECT mailer_id FROM unsubscribes GROUP BY mailer_id HAVING count(*) < 3 AND count(*) > 0) u INNER JOIN mailers m ON m.id = u.mailer_id AND flags & 32 = 0"]))

(defn- names-map []
  (select-name-map ["SELECT id, name FROM mailers WHERE flags & 32 = 0"]))

(defn- names-map-sized [limit]
  (select-name-map ["SELECT id, name FROM mailers WHERE flags & 32 = 0 LIMIT ?" limit]))

(defn- name-frequencies [name-map opts]
  (let [min-ngram (get opts :min-ngram 3)
        max-ngram (get opts :max-ngram 7)]
    (persistent!
     (reduce
      (fn [fmap [id name]]
        (reduce (fn [m n] (assoc! m n (conj (get m n #{}) id)))
                fmap
                (ngrams name min-ngram max-ngram)))
      (transient {})
      name-map))))

(defn- filtered-name-frequencies
  ([name-freqs] (filtered-name-frequencies name-freqs 1))
  ([name-freqs min-limit]
     (delete-if name-freqs (fn [[k v]] (> (count v) min-limit)))))

(defn compare-names
  ([nmap f] (compare-names nmap f {}))
  ([nmap f opts]
     (let [n-threads (get opts :n-threads
                          (/ (count nmap)
                             (inc (.availableProcessors (Runtime/getRuntime)))))
           weight-filter-level (get opts :weight-filter-level 1)
           freq-filter-level  (get opts :freq-filter-level 3)
           idfn (comp nmap first)
           fnmap (-> nmap
                     (name-frequencies opts)
                     (filtered-name-frequencies freq-filter-level))]
       (pmap
        (fn [groups]
          (doseq [[id name] groups]
            (let [name-weight (count name)
                  matches
                  (->> (ngrams name)
                       (map (fn [ngram]
                              (for [id (fnmap ngram)]
                                [id (count ngram)])))
                       (filter not-empty)
                       (apply concat)
                       (sort-by first)
                       (partition-by first)
                       (map
                        (fn [pairs]
                          [(ffirst pairs)
                           ;; TODO:
                           ;; Right now, we're scaling ngram *size* by the name size.
                           ;;
                           ;; We should also test using TF*IDF for the given ngram.
                           (->> pairs (map last) (reduce +) ((fn [w] (/ w name-weight))))]))
                       (filter
                        (fn [[id weight]]
                          (> weight weight-filter-level))))]
              (f id name matches))))
        (partition-all n-threads nmap))
       nil)))


(defn -main []
  (do
    (time
     (compare-names (names-map)
                    (fn [id name matches]
                      (do (spit (str "output/" id ".match") (-> matches (vec) (str)))))))
    (shutdown-agents)))