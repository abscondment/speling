(ns speling.web
  (:use [speling duplicate db]
        [compojure core]
        [ring.middleware json-params])
  (:require [speling.db :as db]
            [cheshire.core :as json]))

(def options {})

(def duplicates-values
  (let [nmap (names-map)
        fnmap (names-map-to-frequencies-map nmap options)]
    (atom {:nmap nmap :fnmap fnmap})))

(defn duplicates-for [id]
  (let [dvm @duplicates-values
        fnmap (get dvm :nmap)
        nmap (get dvm :nmap)
        name (get nmap id)]
    (compute-matches name nmap fnmap options)))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defroutes handler
  (GET "/duplicates/:id" [id]
       (json-response
        (duplicates-for id))))

(def app
  (-> handler
      wrap-json-params))