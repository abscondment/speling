(ns speling.duplicate-test
  (:use [speling.duplicate] :reload-all)
  (:use [clojure.test]
        [clojure.pprint]))

(defn- collect-matches [name-map]
  (let [matches (agent {})
        collector (fn [id name m]
                    (if (not-empty m)
                      (let [sorted-ids (->> m (map first) (sort))]
                        (send matches assoc id sorted-ids))))]
    (compare-names name-map
                   collector
                   {:freq-filter-level 1
                    :count-filter-level 1})
    (await matches)
    (deref matches)))


(deftest simple-matches
  (let [name-map {1 "Pennsylvania"
                  2 "Penn's Woods"
                  3 "Carter Subaru"}
        matches (collect-matches name-map)]
    ;; Assert that the 1st two are matches, but the third has no matches.
    (is (= matches
           {1 [1 2]
            2 [1 2]}))))

(deftest complex-matches
  (let [name-map {1 "Pennsylvania"
                  2 "Penn's Woods"
                  3 "Carter Subaru"
                  4 "Carter Volkswadgon"
                  5 "Carter Subaru/Volkswagon"
                  6 "Carter Volkswagon"
                  7 "Penn and Carter"}
        matches (collect-matches name-map)]
    ;;
    (is (= matches
           {1 [1 2 7]
            2 [1 2 7]
            3 [3 4 5 6 7]
            4 [3 4 5 6 7]
            5 [3 4 5 6 7]
            6 [3 4 5 6 7]
            7 [1 2 3 4 5 6 7]}))))


(comment
 (deftest quality-of-matches
   (let [name-map {1 "Brendan Ribera"
                   2 "Brandon Ribera"
                   3 "Brenden Rebera"
                   4 "Jessica Ribera"
                   6 "Jessica Marie Ribera"}])))