(ns tracer.entities.tuple-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx eq)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.tuple :as sut]))

(clojure.test/use-fixtures :once instrument)

(deftest tuple-component-fns
  (let [a (sut/tuple 4.3 -4.2 3.1 1.0)]
    (is (= 4.3 (sut/x a)))
    (is (= -4.2 (sut/y a)))
    (is (= 3.1 (sut/z a)))
    (is (= 1.0 (sut/w a)))))

(deftest a-tuple-with-w-equal-1-is-a-point
  (let [a (sut/tuple 4.3 -4.2 3.1 1.0)]
    (is (eq [4.3 -4.2 3.1 1.0] a))
    (is (sut/point? a))
    (is (not (sut/vector? a)))))

(deftest a-tuple-with-w-equal-0-is-a-vector
  (let [a (sut/tuple 4.3 -4.2 3.1 0.0)]
    (is (eq [4.3 -4.2 3.1 0.0] a))
    (is (sut/vector? a))
    (is (not (sut/point? a)))))

(deftest point-creates-tuples-with-w-equals-1
  (is (eq [4.0 -4.0 3.0 1.0]
          (sut/point 4.0 -4.0 3.0))))

(deftest vector-creates-tuples-with-w-equals-0
  (is (eq [4.0 -4.0 3.0 0.0]
          (sut/vector 4.0 -4.0 3.0))))

(deftest adding-two-tuples
  (let [a1 (sut/tuple 3 -2 5 1)
        a2 (sut/tuple -2 3 1 0)]
    (is (eq (sut/tuple 1 1 6 1)
            (sut/add a1 a2)))))

(deftest subtracting-tuples
  (testing "Subtracting two points"
    (let [p1 (sut/point 3 2 1)
          p2 (sut/point 5 6 7)]
      (is (eq (sut/vector -2 -4 -6 )
              (sut/sub p1 p2)))))

  (testing "Subtracting a vector from a point"
    (let [p (sut/point 3 2 1)
          v (sut/vector 5 6 7)]
      (is (eq (sut/point -2 -4 -6)
              (sut/sub p v)))))

  (testing "Subtracting two vectors"
    (let [v1 (sut/vector 3 2 1)
          v2 (sut/vector 5 6 7)]
      (is (eq (sut/vector -2 -4 -6 )
              (sut/sub v1 v2)))))

  (testing "Subtracting a vector from the zero vector"
    (let [zero (sut/vector 0 0 0 )
          v (sut/vector 1 -2 3)]
      (is (eq (sut/vector -1 2 -3)
              (sut/sub zero v))))))

(deftest negating-a-tuple
  (let [t (sut/tuple 1 -2 3 -4)]
    (is (eq (sut/tuple -1 2 -3 4)
            (sut/negate t)))))

(deftest multiplying-tuples
  (testing "Multiplying a tuple by a scaler"
    (let [t (sut/tuple 1 -2 3 -4)]
      (is (eq (sut/tuple 3.5 -7.0 10.5 -14.0)
              (sut/mul t 3.5)))))

  (testing "Multiplying a tuple by a fraction"
    (let [t (sut/tuple 1 -2 3 -4)]
      (is (eq (sut/tuple 0.5 -1.0 1.5 -2.0)
              (sut/mul t 0.5))))))

(deftest dividing-tuples
  (testing "Dividing a tuple by a scalar"
    (let [t (sut/tuple 1 -2 3 -4)]
      (is (eq (sut/tuple 0.5 -1.0 1.5 -2.0)
              (sut/div t 2))))))

(deftest magnitude
  (is (= 1.0 (sut/magnitude (sut/vector 1 0 0))))
  (is (= 1.0 (sut/magnitude (sut/vector 0 1 0))))
  (is (= 1.0 (sut/magnitude (sut/vector 0 0 1))))

  (is (= (Math/sqrt 14.0) (sut/magnitude (sut/vector 1 2 3))))
  (is (= (Math/sqrt 14.0) (sut/magnitude (sut/vector -1 -2 -3)))))

(deftest normalising-tuples
  (is (eq (sut/vector 1 0 0)
          (sut/normalise (sut/vector 4 0 0))))
  (is (approx (sut/vector 0.26726 0.53452 0.80178)
              (sut/normalise (sut/vector 1 2 3)))))

(deftest the-magnitude-of-a-normalised-vector
  (let [v (sut/vector 1 2 3)
        norm (sut/normalise v)]
    (is (= 1.0 (sut/magnitude norm)))))

(deftest the-dot-product-of-two-tuples
  (let [a (sut/vector 1 2 3)
        b (sut/vector 2 3 4)]
    (is (= 20.0 (sut/dot a b)))
    (is (= 20.0 (sut/dot b a)))))

(deftest the-cross-product-of-two-vectors
  (let [a (sut/vector 1 2 3)
        b (sut/vector 2 3 4)]
    (is (eq (sut/cross a b)
            (sut/vector -1 2 -1)))
    (is (eq (sut/cross b a)
            (sut/vector 1 -2 1)))))

(deftest reflecting-vectors
  (testing "approaching at 45 degrees"
    (let [v (sut/vector 1 -1 0)
          n (sut/vector 0 1 0)]
      (is (eq (sut/vector 1 1 0)
              (sut/reflect v n)))))

  (testing "off a slanted surface"
    (let [v (sut/vector 0 -1 0)
          root-2-over-2 (/ (Math/sqrt 2) 2)
          n (sut/vector root-2-over-2 root-2-over-2 0)]
      (is (approx (sut/vector 1 0 0)
                  (sut/reflect v n))))))
