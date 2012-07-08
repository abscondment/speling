(ns speling.web
  (:use [speling duplicate db]
        [compojure core]
        [ring.middleware json-params])
  (:require [speling.db :as db]
            [cheshire.core :as json]))

(def options
  {})

(def duplicates-values
  (let [nmap (names-map)
        fnmap (names-map-to-frequencies-map nmap options)]
    {:nmap nmap :fnmap fnmap}))

(defn duplicates-for [name]
  (compare-names
   (get duplicates-values :nmap)
   (get duplicates-values :fnmap)))

(defn json-response [data & [status]]
  {:status (or status 200)
   :headers {"Content-Type" "application/json"}
   :body (json/generate-string data)})

(defroutes handler
  (GET "/" []
       (json-response {"hello" "world"}))

  (PUT "/" [name]
       (json-response {"hello" name})))

(def app
  (-> handler
      wrap-json-params))