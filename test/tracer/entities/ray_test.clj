(ns tracer.entities.ray-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.entities.ray :as ray]))

(clojure.test/use-fixtures :once instrument)

(deftest creating-and-querying-a-ray
  (let [origin (tup/point 1 2 3)
        direction (tup/vector 4 5 6)]
    (testing "it can be created with default ttl"
      (let [r (ray/ray origin direction)]
        (is (= origin (:origin r)))
        (is (= direction (:direction r)))
        (is (= 5 (:ttl r)))))
    (testing "it can be created with a different ttl"
      (let [r (ray/ray origin direction 45)]
        (is (= origin (:origin r)))
        (is (= direction (:direction r)))
        (is (= 45 (:ttl r)))))))

(deftest computing-a-point-from-a-distance
  (let [r (ray/ray (tup/point 2 3 4) (tup/vector 1 0 0))]
    (is (= (tup/point 2 3 4)
           (ray/position r 0)))
    (is (= (tup/point 3 3 4)
           (ray/position r 1)))
    (is (= (tup/point 1 3 4)
           (ray/position r -1)))
    (is (= (tup/point 4.5 3 4)
           (ray/position r 2.5)))))

(deftest transforming-a-ray
  (let [r (ray/ray (tup/point 1 2 3) (tup/vector 0 1 0))]
    (testing "translating"
      (is (= {:origin (tup/point 4 6 8)
              :direction (tup/vector 0 1 0)
              :ttl 5}
             (ray/transform r (transform/translation 3 4 5)))))

    (testing "scaling"
      (is (= {:origin (tup/point 2 6 12)
              :direction (tup/vector 0 3 0)
              :ttl 5}
             (ray/transform r (transform/scaling 2 3 4)))))))
