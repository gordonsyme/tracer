(ns tracer.entities.intersection-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.intersection :as i]
            [tracer.entities.ray :as ray]
            [tracer.entities.plane :as plane]
            [tracer.entities.shape :as shape]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest an-intersection-encapsulates-t-and-object
  (let [s (sphere/sphere)
        i (i/intersection 3.5 s)]
    (is (= 3.5 (:t i)))
    (is (= s (:object i)))))

(deftest aggregating-intersections
  (let [s (sphere/sphere)
        i1 (i/intersection 1 s)
        i2 (i/intersection 2 s)
        xs (i/intersections i1 i2)]
    (is (= [i1 i2] xs))))

(deftest hits
  (testing "when all intersections have positive t"
    (let [s (sphere/sphere)
          i1 (i/intersection 1 s)
          i2 (i/intersection 2 s)
          xs (i/intersections i1 i2)]
      (is (= i1 (i/hit xs)))))

  (testing "when some intersections have negative t"
    (let [s (sphere/sphere)
          i1 (i/intersection -1 s)
          i2 (i/intersection 1 s)
          xs (i/intersections i1 i2)]
      (is (= i2 (i/hit xs)))))

  (testing "when all intersections have negative t"
    (let [s (sphere/sphere)
          i1 (i/intersection -2 s)
          i2 (i/intersection -1 s)
          xs (i/intersections i1 i2)]
      (is (nil? (i/hit xs))))))

(deftest the-hit-is-always-the-lowest-nonnegative-intersection
  (let [s (sphere/sphere)
        i1 (i/intersection 5 s)
        i2 (i/intersection 7 s)
        i3 (i/intersection -3 s)
        i4 (i/intersection 2 s)
        xs (i/intersections i1 i2 i3 i4)]
    (is (= i4 (i/hit xs)))))

(deftest precomputing-the-state-of-an-intersection
  (let [r (ray/ray (tup/point 0 0 -5) (tup/vector 0 0 1) 4)
        shape (sphere/sphere)
        i (i/intersection 4 shape)
        comps (i/prepare-computations i r)]
    (is (= {:t (:t i)
            :object (:object i)
            :point (tup/point 0 0 -1)
            :over-point (tup/point 0.0 0.0 -1.00000001)
            :eyev (tup/vector 0 0 -1)
            :normalv (tup/vector 0 0 -1)
            :reflectv (tup/vector 0 0 -1)
            :inside false
            :ttl 3}
           comps))))

(deftest precomputing-the-state-of-an-intersection-when-the-hit-is-on-the-inside
  (let [r (ray/ray (tup/point 0 0 0) (tup/vector 0 0 1) 4)
        shape (sphere/sphere)
        i (i/intersection 1 shape)
        comps (i/prepare-computations i r)]
    (is (= {:t (:t i)
            :object (:object i)
            :point (tup/point 0 0 1)
            :over-point (tup/point 0.0 0.0 0.99999999)
            :eyev (tup/vector 0 0 -1)
            :normalv (tup/vector 0 0 -1)
            :reflectv (tup/vector 0 0 -1)
            :inside true
            :ttl 3}
           comps))))

(deftest the-hit-should-offset-the-point
  (let [r (ray/ray (tup/point 0 0 -5) (tup/vector 0 0 1))
        shape (shape/with-transform (sphere/sphere)
                                    (transform/translation 0 0 1))
        i (i/intersection 5 shape)
        comps (i/prepare-computations i r)]
    (is (< (tup/z (:over-point comps))
           0.00001))
    (is (> (tup/z (:point comps))
           (tup/z (:over-point comps))))))

(deftest precomputing-the-reflection-vector
  (let [root-2-over-2 (/ (Math/sqrt 2) 2)
        shape (plane/plane)
        r (ray/ray (tup/point 0 1 -1)
                   (tup/vector 0 (- root-2-over-2) root-2-over-2))
        i (i/intersection (Math/sqrt 2) shape)
        comps (i/prepare-computations i r)]
    (is (= (tup/vector 0 root-2-over-2 root-2-over-2)
           (:reflectv comps)))))
