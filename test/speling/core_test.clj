(ns speling.core-test
  (:use [speling.core] :reload-all)
  (:use [clojure.test]))

(deftest words-test
  (is (= (words "I can't imagine you without this.Nikolai Rimsky-Korsakov$$$")
         ["i" "can't" "imagine" "you" "without" "this" "nikolai" "rimsky-korsakov"])))

