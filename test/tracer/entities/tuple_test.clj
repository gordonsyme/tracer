(ns tracer.entities.tuple-test
  (:require [clojure.test :refer (deftest testing is)]
            [clojure.spec.test.alpha :as s-test]
            [tracer.entities.tuple :as sut]))

(clojure.test/use-fixtures :once (fn [t]
                                   (s-test/instrument)
                                   (t)
                                   (s-test/unstrument)))

(deftest a-tuple-with-w-equal-1-is-a-point
  (let [a (sut/tuple 4.3 -4.2 3.1 1.0)]
    (is (= [4.3 -4.2 3.1 1.0] a))
    (is (sut/point? a))
    (is (not (sut/vector? a)))))

(deftest a-tuple-with-w-equal-0-is-a-vector
  (let [a (sut/tuple 4.3 -4.2 3.1 0.0)]
    (is (= [4.3 -4.2 3.1 0.0] a))
    (is (sut/vector? a))
    (is (not (sut/point? a)))))

(deftest point-creates-tuples-with-w-equals-1
  (is (= [4.0 -4.0 3.0 1.0]
         (sut/point 4.0 -4.0 3.0))))

(deftest vector-creates-tuples-with-w-equals-0
  (is (= [4.0 -4.0 3.0 0.0]
         (sut/vector 4.0 -4.0 3.0))))

(deftest adding-two-tuples
  (let [a1 (sut/tuple 3 -2 5 1)
        a2 (sut/tuple -2 3 1 0)]
    (is (= (sut/tuple 1 1 6 1)
           (sut/add a1 a2)))))
