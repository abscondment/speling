(ns speling.autocorrect-test
  (:use [speling.autocorrect] :reload-all)
  (:use [clojure.test]
        [clojure.pprint]))

(deftest train-values
  (is (= (train ["foo" "bar"])
         {"foo" 2 "bar" 2}))
  (is (= (train (repeat 10 "foo"))
         {"foo" 11}))
  (is (= (train ["foo" "bar"] 10)
         {"foo" 11 "bar" 11}))
  (is (= (train ["foo" "bar" "bar" "baz" "baz" "baz"] 3)
         {"foo" 4 "bar" 7 "baz" 10})))

(deftest train-map-values
  ;; TODO
  (is (true? true)))