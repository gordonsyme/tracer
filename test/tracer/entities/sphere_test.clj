(ns tracer.entities.sphere-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.intersection :as i]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as ray]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest a-ray-intersects-a-sphere-at-two-points
  (testing "two distinct points"
    (let [r (ray/ray (tup/point 0 0 -5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t 4.0 :object s}
              {:t 6.0 :object s}]
             (i/intersect s r)))))

  (testing "a tangent"
    (let [r (ray/ray (tup/point 0 1 -5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t 5.0 :object s} {:t 5.0 :object s}]
             (i/intersect s r)))))

  (testing "the ray misses"
    (let [r (ray/ray (tup/point 0 2 -5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= []
             (i/intersect s r)))))

  (testing "the ray originates inside the sphere"
    (let [r (ray/ray (tup/point 0 0 0)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t -1.0 :object s} {:t 1.0 :object s}]
             (i/intersect s r)))))

  (testing "the sphere is behind the ray"
    (let [r (ray/ray (tup/point 0 0 5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t -6.0 :object s} {:t -4.0 :object s}]
             (i/intersect s r))))))

(deftest sphere-transforms
  (let [s (sphere/sphere)]
    (testing "default transform"
      (is (= (mat/identity 4)
             (:transform s))))
    (testing "setting a transform"
      (let [tr (transform/translation 1 2 3)
            s (sphere/with-transform s tr)]
        (is (= tr (:transform s)))))))

(deftest intersecting-a-scaled-sphere-with-a-ray
  (let [r (ray/ray (tup/point 0 0 -5)
                   (tup/vector 0 0 1))
        s (sphere/with-transform (sphere/sphere)
                                 (transform/scaling 2 2 2))]
    (is (= [{:t 3.0 :object s} {:t 7.0 :object s}]
           (i/intersect s r)))))

(deftest intersecting-a-translated-sphere-with-a-ray
  (let [r (ray/ray (tup/point 0 0 -5)
                   (tup/vector 0 0 1))
        s (sphere/with-transform (sphere/sphere)
                                 (transform/translation 5 0 0))]
    (is (= [] (i/intersect s r)))))
