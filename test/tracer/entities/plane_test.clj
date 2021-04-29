(ns tracer.entities.plane-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.plane :as plane]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest the-normal-of-a-plane-is-constant-everywhere
  (let [p (plane/plane)]
    (is (= (tup/vector 0 1 0)
           (shape/local-normal-at p (tup/point 0 0 0))))

    (is (= (tup/vector 0 1 0)
           (shape/local-normal-at p (tup/point 10 0 -10))))

    (is (= (tup/vector 0 1 0)
           (shape/local-normal-at p (tup/point -5 0 150))))))

(deftest plane-intersections
  (testing "intersect with a ray parallel to the plane"
    (let [p (plane/plane)
          r (ray/ray (tup/point 0 10 0) (tup/vector 0 0 1))]
      (is (empty? (shape/local-intersect (shape/relations) p r)))))

  (testing "intersect with a coplanar ray"
    (let [p (plane/plane)
          r (ray/ray (tup/point 0 0 0) (tup/vector 0 0 1))]
      (is (empty? (shape/local-intersect (shape/relations) p r)))))

  (testing "a ray intersecting a plane from above"
    (let [p (plane/plane)
          r (ray/ray (tup/point 0 1 0) (tup/vector 0 -1 0))]
      (is (= [1.0] (map :t (shape/local-intersect (shape/relations) p r))))))

  (testing "a ray intersecting a plane from below"
    (let [p (plane/plane)
          r (ray/ray (tup/point 0 -1 0) (tup/vector 0 1 0))]
      (is (= [1.0] (map :t (shape/local-intersect (shape/relations) p r)))))))
