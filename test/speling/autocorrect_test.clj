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
  (is (= (train-map [{:words ["foo"] :count 1}])
         {"foo" 2}))
  (is (= (train-map  [{:words ["foo"] :count 1}
                      {:words ["foo" "bar"] :count 5}])
         {"foo" 7 "bar" 6}))
  (is (= (train-map (take 10
                          (repeat {:words ["foo" "bar" "baz"] :count 1})))
         {"foo" 11 "bar" 11 "baz" 11}))
  (is (= (train-map [{:words ["foo"] :count 4}
                     {:words ["foo" "bar"] :count 2}
                     {:words ["foo" "bar" "baz"] :count 1.25}
                     {:words ["foo" "bar" "baz" "bingo"] :count 1}])
         {"foo" (+ 1 1 1.25 2 4)
          "bar" (+ 1 1 1.25 2)
          "baz" (+ 1 1 1.25)
          "bingo" (+ 1 1)})))