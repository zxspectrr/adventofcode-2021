(ns advent-2021.day5-test
  (:require [clojure.test :refer :all]
            [advent-2021.day5 :refer :all]))

(deftest line-matches-point
  (testing "horizontal line match"
    (is (= true
           (line-matches-point? {:start {:x 0, :y 9}
                                 :end {:x 5, :y 9}}
                                {:x 3 :y 9}))))

  (testing "horizontal line miss"
    (is (= false
           (line-matches-point? {:start {:x 0, :y 9}
                                 :end {:x 5, :y 9}}
                                {:x 6 :y 9}))))

  (testing "vertical line match"
    (is (= true
           (line-matches-point? {:start {:x 5, :y 4}
                                 :end {:x 5, :y 9}}
                                {:x 5 :y 7}))))
  (testing "vertical line miss"
    (is (= false
           (line-matches-point? {:start {:x 5, :y 4}
                                 :end {:x 5, :y 9}}
                                {:x 5 :y 3})))))
