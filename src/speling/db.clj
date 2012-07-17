(ns speling.db
  (:require [clojure.java.jdbc :as sql]))

(def db {:subprotocol "mysql"
         :subname "//127.0.0.1:3306/paperkarma_development"
         :user "root"
         :password nil})

(defn select-map [query-coll]
  (sql/with-connection db
    (sql/with-query-results rows
      query-coll
      (reduce
       (fn [m row] (assoc m (:id row) row))
       {} rows))))

(defn select-name-map [query-coll]
  (sql/with-connection db
    (sql/with-query-results rows
      query-coll
      (reduce
       (fn [m row] (assoc m (:id row) (:name row)))
       {} rows))))

(defn new-names-map []
  (select-name-map ["SELECT m.id, m.name FROM (SELECT mailer_id FROM unsubscribes GROUP BY mailer_id HAVING count(*) < 3 AND count(*) > 0) u INNER JOIN mailers m ON m.id = u.mailer_id AND flags & 32 = 0"]))

(defn active-names-counts-map []
  (let [names (select-map ["SELECT id, name, url, email, company_url, 0 AS count FROM mailers WHERE active = 1 AND flags & 32 = 0"])
        counts (select-map ["SELECT m.id AS id, count(*) AS count FROM unsubscribes u INNER JOIN mailers m ON m.active = 1 AND m.flags & 32 = 0 AND m.id = u.mailer_id GROUP BY m.id"])]
    (merge-with merge names counts)))

(defn active-names-map []
  (select-name-map ["SELECT id, name FROM mailers WHERE active = 1 AND flags & 32 = 0"]))

(defn names-map []
  (select-name-map ["SELECT id, name FROM mailers WHERE flags & 32 = 0"]))

(defn names-map-sized [limit]
  (select-name-map ["SELECT id, name FROM mailers WHERE flags & 32 = 0 LIMIT ?" limit]))