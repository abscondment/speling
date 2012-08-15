(ns speling.autocorrect-test
  (:use [speling.autocorrect] :reload-all)
  (:use [clojure.test]
        [clojure.pprint]))

(deftest train-values
  ;; words only
  (is (= (train ["foo" "bar"])
         {"foo" 2 "bar" 2}))
  
  (is (= (train (repeat 10 "foo"))
         {"foo" 11}))

  ;; words and weight
  (is (= (train ["foo" "bar"] 10)
         {"foo" 11 "bar" 11}))
  
  (is (= (train ["foo" "bar" "bar" "baz" "baz" "baz"] 3)
         {"foo" 4 "bar" 7 "baz" 10}))
  
  ;; words, weight, and existing model
  (is (= (train ["foo" "bar"]
                10
                {"foo" 100})
         {"foo" 110 "bar" 11}))
  
  (is (= (train ["foo" "bar" "bar" "baz" "baz" "baz"]
                3
                {"foo" 3 "bar" 4 "baz" 0})
         
         {"foo" 6 "bar" 10 "baz" 9})))

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
         {"foo"   (+ 1 1 1.25 2 4)
          "bar"   (+ 1 1 1.25 2)
          "baz"   (+ 1 1 1.25)
          "bingo" (+ 1 1)})))

(deftest correct-results
  (let [_ (swap-nwords "foo bar bar baz bingo")
        input {"fool" "foo" ;; single edit
               "binge" "bingo" ;; single edit
               "bigot" "bingo" ;; two edits
               "bat" "bar" ;; weight-based choice
               "haz" "baz" ;; another single edit
               "marmelade" nil ;; too many changes
               }]
    (doseq [[original correction] input]
      (is (= (first (correct original)) correction)))))

