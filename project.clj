(defproject speling "0.5.0-SNAPSHOT"
  :description "Got speling?"
  :dependencies [[org.clojure/clojure "1.4.0"]

                 ;; data access
                 [org.clojure/java.jdbc "0.2.3"]
                 [mysql/mysql-connector-java "5.1.20"]

                 ;; web tier
                 [compojure "1.1.0"]
                 [cheshire "4.0.0"]
                 [ring/ring-jetty-adapter "1.1.1"]
                 [fuziontech/ring-json-params "0.2.0"]])

