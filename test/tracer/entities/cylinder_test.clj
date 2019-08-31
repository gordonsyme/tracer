(ns tracer.entities.cylinder-test
  (:require [clojure.test :refer (are deftest is)]
            [tracer.fixtures :refer (instrument)]
            [tracer.comparators :refer (approx eq)]
            [tracer.entities.cylinder :as cylinder]
            [tracer.entities.intersection :as i]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest a-ray-misses-a-cylinder
  (let [c (cylinder/cylinder)]
    (are [origin direction]
         (let [r (ray/ray origin direction)
               xs (i/intersect c r)]
           (empty? xs))
      (tup/point 1 0 0) (tup/vector 0 1 0)
      (tup/point 0 0 0) (tup/vector 0 1 0)
      (tup/point 0 0 -5) (tup/vector 1 1 1))))

(deftest a-ray-strikes-a-cylinder
  (let [c (cylinder/cylinder)]
    (are [origin direction t1 t2]
         (let [r (ray/ray origin (tup/normalise direction))
               xs (i/intersect c r)]
           (approx [(double t1) (double t2)]
                   (map :t xs)))
      ;; origin direction t1 t2
      (tup/point 1 0 -5) (tup/vector 0 0 1) 5 5
      (tup/point 0 0 -5) (tup/vector 0 0 1) 4 6
      (tup/point 0.5 0 -5) (tup/vector 0.1 1 1) 6.80798 7.08872)))

(deftest normal-vector-on-a-cylinder
  (let [c (cylinder/cylinder)]
    (are [p expected]
         (eq expected
             (shape/local-normal-at c p))
      (tup/point 1 0 0) (tup/vector 1 0 0)
      (tup/point 0 5 -1) (tup/vector 0 0 -1)
      (tup/point 0 -2 1) (tup/vector 0 0 1)
      (tup/point -1 1 0) (tup/vector -1 0 0))))

(deftest the-default-maximum-and-minimum-for-a-cylinder
  (let [c (cylinder/cylinder)]
    (is (and (Double/isInfinite (:minimum c))
             (neg? (:minimum c))))
    (is (and (Double/isInfinite (:maximum c))
             (pos? (:maximum c))))))

(deftest intersecting-a-constrained-cylinder
  (let [c (-> (cylinder/cylinder)
              (cylinder/with-minimum 1)
              (cylinder/with-maximum 2))]
    (are [point direction num-xs]
         (let [r (ray/ray point (tup/normalise direction))
               xs (i/intersect c r)]
           (= num-xs (count xs)))
      (tup/point 0 1.5 0) (tup/vector 0.1 1 0) 0
      (tup/point 0 3 -5) (tup/vector 0 0 1) 0
      (tup/point 0 0 -5) (tup/vector 0 0 1) 0
      (tup/point 0 2 -5) (tup/vector 0 0 1) 0
      (tup/point 0 1 -5) (tup/vector 0 0 1) 0
      (tup/point 0 1.5 -2) (tup/vector 0 0 1) 2)))

(deftest the-default-closed-value-for-a-cylinder
  (is (false? (:closed (cylinder/cylinder)))))

(deftest intersecting-the-caps-of-a-closed-cylinder
  (let [c (-> (cylinder/cylinder)
              (cylinder/with-minimum 1)
              (cylinder/with-maximum 2)
              (cylinder/with-closed true))]
    (are [point direction num-xs]
         (let [r (ray/ray point (tup/normalise direction))
               xs (i/intersect c r)]
           (= num-xs (count xs)))
      (tup/point 0  3  0) (tup/vector 0 -1 0) 2
      (tup/point 0  3 -2) (tup/vector 0 -1 2) 2
      (tup/point 0  4 -2) (tup/vector 0 -1 1) 2
      (tup/point 0  0 -2) (tup/vector 0  1 2) 2
      (tup/point 0 -1 -2) (tup/vector 0  1 1) 2)))

(deftest the-normal-vector-on-a-cylinder's-end-caps
  (let [c (-> (cylinder/cylinder)
              (cylinder/with-minimum 1)
              (cylinder/with-maximum 2)
              (cylinder/with-closed true))]
    (are [p expected]
         (eq expected (shape/local-normal-at c p))
      (tup/point 0   1 0)   (tup/vector 0 -1 0)
      (tup/point 0.5 1 0)   (tup/vector 0 -1 0)
      (tup/point 0   1 0.5) (tup/vector 0 -1 0)
      (tup/point 0   2 0)   (tup/vector 0 1 0)
      (tup/point 0.5 2 0)   (tup/vector 0 1 0)
      (tup/point 0   2 0.5) (tup/vector 0 1 0))))
