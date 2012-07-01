(ns speling.duplicate-test
  (:use [speling.duplicate] :reload-all)
  (:use [clojure.test]
        [clojure.pprint]))
(comment
 (def name-map
   {1 "Pennsylvania"
    2 "Penn's Woods"
    3 "Carter Subaru"
    4 "Carter Volkswadgon"
    5 "Carter Subaru/Volkswagon"
    6 "Carter Volkswagon"
    7 "Penn and Teller"}
   ))

(deftest make-co
  (let [name-map {1 "Pennsylvania"
                  2 "Penn's Woods"
                  3 "Carter Subaru"}
        co (co-occurring-ids-map name-map 1)]
    (is (= co {1 {2 6} 2 {1 6}}))))
