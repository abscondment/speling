(ns speling.duplicate-test
  (:use [speling.duplicate] :reload-all)
  (:use [clojure.test]
        [clojure.pprint]))

(defn- co-map-without-counts [co-map]
  (sort-by first
   (map (fn [[k v]] [k (sort (map first v))])
        co-map)))

(deftest simple-matches
  (let [name-map {1 "Pennsylvania"
                  2 "Penn's Woods"
                  3 "Carter Subaru"}
        matches (co-occurring-ids-map name-map 1)]
    ;; Assert that the 1st two are matches, but the third has no matches.
    (is (= (co-map-without-counts matches)
           [[1 [2]] [2 [1]]]))))

(deftest complex-matches
  (let [name-map {1 "Pennsylvania"
                  2 "Penn's Woods"
                  3 "Carter Subaru"
                  4 "Carter Volkswadgon"
                  5 "Carter Subaru/Volkswagon"
                  6 "Carter Volkswagon"
                  7 "Penn and Carter"}
        matches (co-occurring-ids-map name-map 1)]
    ;;
    (is (= (co-map-without-counts matches)
           [[1 [2 7]]
            [2 [1 7]]
            [3 [4 5 6 7]]
            [4 [3 5 6 7]]
            [5 [3 4 6 7]]
            [6 [3 4 5 7]]
            [7 [1 2 3 4 5 6]]]))))

(deftest quality-of-matches
  (let [name-map {1 "Brendan Ribera"
                  2 "Brandon Ribera"
                  3 "Brenden Rebera"
                  4 "Jessica Ribera"
                  6 "Jessica Marie Ribera"}]))