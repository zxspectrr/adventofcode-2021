(ns advent-2021.day5-test
  (:require [clojure.test :refer :all]
            [advent-2021.day5 :refer :all]))

(deftest line-intersect-points
  (testing "horitonzal line overlap"
    (let [line1 {:start {:x 1 :y 1} :end {:x 3 :y 1} :type :horizontal}
          line2 {:start {:x 2 :y 1} :end {:x 4 :y 1} :type :horizontal}]
      (is (= #{{ :x 2 :y 1} {:x 3 :y 1}}
             (find-intersections-for-lines line1 line2)))))

  (testing "vertical line overlap"
    (let [line1 {:start {:x 1 :y 1} :end {:x 1 :y 3} :type :vertical}
          line2 {:start {:x 1 :y 2} :end {:x 1 :y 4} :type :vertical}]
      (is (= #{{ :x 1 :y 2} {:x 1 :y 3}}
             (find-intersections-for-lines line1 line2)))))

  (testing "horizontal vertical line overlap"
    (let [line1 {:start {:x 1 :y 1} :end {:x 3 :y 1} :type :horizontal}
          line2 {:start {:x 2 :y 0} :end {:x 2 :y 4} :type :vertical}]
      (is (= #{{ :x 2 :y 1}}
             (find-intersections-for-lines line1 line2)))))

  (testing "vertical horizontal line overlap"
    (let [line1 {:start {:x 2 :y 0} :end {:x 2 :y 4} :type :vertical}
          line2 {:start {:x 1 :y 1} :end {:x 3 :y 1} :type :horizontal}]
      (is (= #{{ :x 2 :y 1}}
             (find-intersections-for-lines line1 line2))))))

(deftest all-line-intersections
  (testing "all intersections"
    (let [lines (straight-lines)
          (find-intersection-points lines)])))

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
