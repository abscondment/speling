(ns speling.web
  (:use [speling duplicate db]
        [compojure core]
        [ring.middleware json-params]
        [ring.adapter jetty])
  (:require [speling.db :as db]
            [cheshire.core :as json]))

(def options {})

(def duplicates-values
  (atom
   (let [nmap (names-map)
         fnmap (names-map-to-frequencies-map nmap options)]
     {:nmap nmap :fnmap fnmap})))

(defn duplicates-for
  ([id] (duplicates-for id {}))
  ([id {min-weight :min-weight limit :limit}]
     (let [dvm @duplicates-values
           id (try
                (Integer/parseInt id)
                (catch NumberFormatException e nil))
           fnmap (get dvm :fnmap)
           nmap (get dvm :nmap)
           name (get nmap id)]
       (if (not-empty name)
         (->> (compute-matches name nmap fnmap options)
              ((fn [coll]
                 (if (nil? min-weight)
                   coll
                   (filter #(>= (last %) min-weight) coll))))
              (sort-by last)
              (reverse)
              ((fn [coll]
                 (if (nil? limit)
                   coll
                   (take limit coll))))
              (map
               (fn [[id weight]]
                 {:id id, :name (get nmap id), :weight (double weight)})))))))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defroutes handler
  (GET "/duplicates/:id" [id]
       (let [result (duplicates-for id)
             status (if (nil? result) 404 200)]
         (json-response result status))))

(def app
  (-> handler
      wrap-json-params))

;; TODO:
;;  * configuration (port, logging)
;;  * logging
;;
(defn -main [] (run-jetty app {:port 9000}))
