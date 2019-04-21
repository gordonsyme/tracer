(ns tracer.entities.colour-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.colour :as sut]))

(clojure.test/use-fixtures :once instrument)

(deftest colours-are-rgb-tuples
  (let [c (sut/colour -0.5 0.4 1.7)]
    (is (= [-0.5 0.4 1.7]
           c))
    (is (= -0.5 (sut/red c)))
    (is (= 0.4 (sut/green c)))
    (is (= 1.7 (sut/blue c)))))

(deftest adding-colours
  (let [c1 (sut/colour 0.9 0.6 0.75)
        c2 (sut/colour 0.7 0.1 0.25)]
    (is (= (sut/colour 1.6 0.7 1.0)
           (sut/add c1 c2)))))

(deftest subtracting-colours
  (let [c1 (sut/colour 0.9 0.6 0.75)
        c2 (sut/colour 0.7 0.1 0.25)]
    (is (approx (sut/colour 0.2 0.5 0.5)
                (sut/sub c1 c2)))))

(deftest multiplying-colours-by-a-scalar
  (let [c1 (sut/colour 0.2 0.3 0.4)]
    (is (= (sut/colour 0.4 0.6 0.8)
           (sut/mul c1 2)))))

(deftest multiplying-colours
  (let [c1 (sut/colour 1 0.2 0.4)
        c2 (sut/colour 0.9 1 0.1)]
    (is (approx (sut/colour 0.9 0.2 0.04)
                (sut/hadamard c1 c2)))))
