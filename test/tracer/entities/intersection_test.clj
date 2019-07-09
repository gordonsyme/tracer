(ns tracer.entities.intersection-test
  (:require [clojure.test :refer (deftest testing is are)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.intersection :as i]
            [tracer.entities.ray :as ray]
            [tracer.entities.material :as material]
            [tracer.entities.plane :as plane]
            [tracer.entities.shape :as shape]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(def ^:private glass
  (-> (material/material)
      (material/with-transparency 1.0)
      (material/with-refractive-index 1.5)))

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
            :under-point (tup/point 0.0 0.0 -0.99999999)
            :eyev (tup/vector 0 0 -1)
            :normalv (tup/vector 0 0 -1)
            :reflectv (tup/vector 0 0 -1)
            :inside false
            :ttl 3
            :n1 1
            :n2 1}
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
            :under-point (tup/point 0.0 0.0 1.00000001)
            :eyev (tup/vector 0 0 -1)
            :normalv (tup/vector 0 0 -1)
            :reflectv (tup/vector 0 0 -1)
            :inside true
            :ttl 3
            :n1 1
            :n2 1}
           comps))))

(deftest the-hit-should-offset-the-point
  (let [r (ray/ray (tup/point 0 0 -5) (tup/vector 0 0 1))
        shape (shape/with-transform (sphere/sphere)
                                    (transform/translation 0 0 1))
        i (i/intersection 5 shape)
        comps (i/prepare-computations i r)]
    (is (> (tup/z (:over-point comps))
           -0.00001))
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

(deftest finding-n1-and-n2-at-various-intersections
  (let [a (-> (sphere/sphere)
              (shape/with-transform (transform/scaling 2 2 2))
              (shape/with-material glass))
        b (-> (sphere/sphere)
              (shape/with-transform (transform/translation 0 0 -0.25))
              (shape/with-material
                (material/with-refractive-index glass 2.0)))
        c (-> (sphere/sphere)
              (shape/with-transform (transform/translation 0 0 0.25))
              (shape/with-material
                (material/with-refractive-index glass 2.5)))
        r (ray/ray (tup/point 0 0 -4)
                   (tup/vector 0 0 1))
        is (i/intersections
             (i/intersection 2 a)
             (i/intersection 2.75 b)
             (i/intersection 3.25 c)
             (i/intersection 4.75 b)
             (i/intersection 5.25 c)
             (i/intersection 6 a))]
    (are [index n1 n2]
         (let [comps (i/prepare-computations (nth is index) r is)]
           (and (= n1 (:n1 comps))
                (= n2 (:n2 comps))))
      0 1.0 1.5
      1 1.5 2.0
      2 2.0 2.5
      3 2.5 2.5
      4 2.5 1.5
      5 1.5 1.0)))

(deftest the-under-point-is-offset-below-the-surface
  (let [r (ray/ray (tup/point 0 0 -5)
                   (tup/vector 0 0 1))
        shape (-> (sphere/sphere)
                  (shape/with-transform (transform/translation 0 0 1))
                  (shape/with-material glass))
        i (i/intersection 5 shape)
        comps (i/prepare-computations i r (i/intersections i))]
    (is (< (tup/z (:under-point comps))
           0.00001))
    (is (< (tup/z (:point comps))
           (tup/z (:under-point comps))))))

(deftest the-schlick-approximation-under-total-internal-reflection
  (let [root-2-over-2 (/ (Math/sqrt 2) 2)
        shape (shape/with-material (sphere/sphere) glass)
        r (ray/ray (tup/point 0 0 root-2-over-2)
                   (tup/vector 0 1 0))
        xs (i/intersections (i/intersection (- root-2-over-2) shape)
                            (i/intersection root-2-over-2 shape))
        comps (i/prepare-computations (second xs) r xs)]
    (is (= 1.0 (i/schlick-reflectance comps)))))

(deftest the-schlick-approximation-with-a-perpendicular-viewing-angle
  (let [shape (shape/with-material (sphere/sphere) glass)
        r (ray/ray (tup/point 0 0 0)
                   (tup/vector 0 1 0))
        xs (i/intersections (i/intersection -1 shape)
                            (i/intersection 1 shape))
        comps (i/prepare-computations (second xs) r xs)]
    (is (approx 0.04 (i/schlick-reflectance comps)))))

(deftest the-schlick-approximation-with-a-perpendicular-viewing-angle
  (let [shape (shape/with-material (sphere/sphere) glass)
        r (ray/ray (tup/point 0 0.99 -2)
                   (tup/vector 0 0 1))
        xs (i/intersections (i/intersection 1.8589 shape))
        comps (i/prepare-computations (first xs) r xs)]
    (is (approx 0.48873 (i/schlick-reflectance comps)))))
