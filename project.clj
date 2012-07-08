(defproject speling "0.5.0-SNAPSHOT"
  :description "Got speling?"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/java.jdbc "0.2.3"]
                 [mysql/mysql-connector-java "5.1.20"]

                 [ring/ring-jetty-adapter "1.1.1"]
                 [ring-json-params "0.1.3"]
                 [compojure "1.1.0"]
                 [cheshire "4.0.0"]])
