(ns tracer.entities.cone-test
  (:require [clojure.test :refer (are deftest is)]
            [tracer.fixtures :refer (instrument)]
            [tracer.comparators :refer (approx)]
            [tracer.entities.cone :as cone]
            [tracer.entities.intersection :as i]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest intersecting-a-cone-with-a-ray
  (let [c (cone/cone)]
    (are [origin direction t0 t1]
         (let [r (ray/ray origin (tup/normalise direction))
               xs (i/intersect (shape/relations) c r)]
           (approx [(double t0) (double t1)]
                   (map :t xs)))
      (tup/point 0 0 -5) (tup/vector 0 0 1)     5        5
      (tup/point 0 0 -5) (tup/vector 1 1 1)     8.66025  8.66025
      (tup/point 1 1 -5) (tup/vector -0.5 -1 1) 4.55006 49.44994)))

(deftest intersecting-a-cone-with-a-ray-parallel-to-one-of-it's-halves
  (let [c (cone/cone)
        direction (tup/normalise (tup/vector 0 1 1))
        r (ray/ray (tup/point 0 0 -1) direction)
        xs (i/intersect (shape/relations) c r)]
    (is (approx [0.35355] (map :t xs)))))

(deftest the-default-closed-value-for-a-cone
  (is (false? (:closed (cone/cone)))))

(deftest intersecting-a-cone's-end-caps
  (let [c (-> (cone/cone)
              (cone/with-minimum -0.5)
              (cone/with-maximum 0.5)
              (cone/with-closed true))]
    (are [origin direction num-xs]
         (let [r (ray/ray origin (tup/normalise direction))
               xs (i/intersect (shape/relations) c r)]
           (= num-xs (count xs)))
      (tup/point 0 0 -5)    (tup/vector 0 1 0) 0
      (tup/point 0 0 -0.25) (tup/vector 0 1 1) 2
      (tup/point 0 0 -0.25) (tup/vector 0 1 0) 4)))

(deftest computing-the-normal-vector-on-a-cone
  (let [c (cone/cone)]
    (are [p expected]
         (= expected (shape/local-normal-at c p))
      (tup/point  0  0 0) (tup/vector 0 0 0)
      (tup/point  1  1 1) (tup/vector 1 (- (Math/sqrt 2)) 1)
      (tup/point -1 -1 0) (tup/vector -1 1 0))))
