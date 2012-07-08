(ns speling.duplicate
  (:use [speling core]
        [clojure pprint])
  (:require [speling.db :as db]
            [clojure.set :as set]))

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

(defn- filtered-name-frequencies [name-freqs opts]
  (let [min-limit (get opts :freq-filter-level 3)]
    (delete-if name-freqs (fn [[k v]] (> (count v) min-limit)))))

(defn names-map-to-frequencies-map [name-map opts]
  (-> name-map
      (name-frequencies opts)
      (filtered-name-frequencies opts)))

(defn compute-matches [name nmap fnmap opts]
  (let [weight-filter-level (get opts :weight-filter-level 1)
        min-ngram (get opts :min-ngram 3)
        max-ngram (get opts :max-ngram 7)
        name-weight (count name)]
    (->> (ngrams name min-ngram max-ngram)
         (map
          (fn [ngram]
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
            (> weight weight-filter-level))))))

(defn compare-names
  ([nmap f] (compare-names nmap f {}))
  ([nmap f opts] (compare-names
                  nmap
                  (-> nmap
                      (name-frequencies opts)
                      (filtered-name-frequencies opts))
                  f
                  opts))
  ([nmap fnmap f opts]
     (let [n-threads (get opts :n-threads
                          (/ (count nmap)
                             (inc (.availableProcessors (Runtime/getRuntime)))))]
       (doall
        (pmap
         (fn [groups]
           (doseq [[id name] groups]
             (f id name (compute-matches name nmap fnmap opts))))
         (partition-all n-threads nmap)))
       nil)))

(comment
 (defn -main []
   (do
     (time
      (compare-names (db/names-map)
                     (fn [id name matches]
                       (do (spit (str "output/" id ".match") (-> matches (vec) (str)))))))
     (shutdown-agents))))