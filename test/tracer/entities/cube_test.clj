(ns tracer.entities.cube-test
  (:require [clojure.test :refer (deftest are)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.intersection :as i]
            [tracer.entities.cube :as cube]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest a-ray-intersects-a-cube
  (let [c (cube/cube)]
    (are [origin direction t1 t2]
         (let [r (ray/ray origin direction)
               xs (i/intersect c r)]
           (= [(double t1) (double t2)]
              (map :t xs)))
      ;; +/- direction vectors
      (tup/point  5    0.5  0) (tup/vector -1  0  0) 4 6
      (tup/point -5    0.5  0) (tup/vector  1  0  0) 4 6
      (tup/point  0.5  5    0) (tup/vector  0 -1  0) 4 6
      (tup/point  0.5 -5    0) (tup/vector  0  1  0) 4 6
      (tup/point  0.5  0    5) (tup/vector  0  0 -1) 4 6
      (tup/point  0.5  0   -5) (tup/vector  0  0  1) 4 6
      ;; a point inside the cube
      (tup/point 0 0.5 0) (tup/vector 0 0 1) -1 1)))

(deftest a-ray-misses-a-cube
  (let [c (cube/cube)]
    (are [origin direction]
         (let [r (ray/ray origin direction)
               xs (i/intersect c r)]
           (empty? xs))
      ;; +/- direction vectors
      (tup/point -2  0  0) (tup/vector 0.2673 0.5345 0.8018)
      (tup/point  0 -2  0) (tup/vector 0.8018 0.2673 0.5345)
      (tup/point  0  0 -2) (tup/vector 0.5345 0.8018 0.2673)
      (tup/point 2 0 2) (tup/vector  0  0 -1)
      (tup/point 0 2 2) (tup/vector  0 -1  0)
      (tup/point 2 2 0) (tup/vector -1  0  0 ))))

(deftest the-normal-on-the-surface-of-a-cube
  (let [c (cube/cube)]
    (are [point normal]
         (approx normal (shape/local-normal-at c point))
      (tup/point  1    0.5 -0.8) (tup/vector  1  0 0)
      (tup/point -1   -0.2  0.9) (tup/vector -1  0 0)
      (tup/point -0.4  1   -0.1) (tup/vector  0  1 0)
      (tup/point  0.3 -1   -0.7) (tup/vector  0 -1 0)
      (tup/point -0.6  0.3  1) (tup/vector  0 0  1)
      (tup/point  0.4  0.4 -1) (tup/vector  0 0 -1)
      (tup/point  1    1    1) (tup/vector  1 0  0)
      (tup/point -1   -1   -1) (tup/vector -1 0  0))))
