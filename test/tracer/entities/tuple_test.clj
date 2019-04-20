(ns tracer.entities.tuple-test
  (:require [clojure.test :refer (deftest testing is)]
            [orchestra.spec.test :as s-test]
            [tracer.entities.tuple :as sut]))

(clojure.test/use-fixtures :once (fn [t]
                                   (s-test/instrument)
                                   (t)
                                   (s-test/unstrument)))

(defn- approx
  "Compare two tuples approximately"
  [v1 v2]
  (if (and (sequential? v1)
           (sequential? v2))
    (every? true? (map approx v1 v2))
    (< (Math/abs (- v1 v2))
       0.0001)))

(deftest a-tuple-with-w-equal-1-is-a-point
  (let [a (#'sut/tuple 4.3 -4.2 3.1 1.0)]
    (is (= [4.3 -4.2 3.1 1.0] a))
    (is (sut/point? a))
    (is (not (sut/vector? a)))))

(deftest a-tuple-with-w-equal-0-is-a-vector
  (let [a (#'sut/tuple 4.3 -4.2 3.1 0.0)]
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
  (let [a1 (#'sut/tuple 3 -2 5 1)
        a2 (#'sut/tuple -2 3 1 0)]
    (is (= (#'sut/tuple 1 1 6 1)
           (sut/add a1 a2)))))

(deftest subtracting-tuples
  (testing "Subtracting two points"
    (let [p1 (sut/point 3 2 1)
          p2 (sut/point 5 6 7)]
      (is (= (sut/vector -2 -4 -6 )
             (sut/sub p1 p2)))))

  (testing "Subtracting a vector from a point"
    (let [p (sut/point 3 2 1)
          v (sut/vector 5 6 7)]
      (is (= (sut/point -2 -4 -6)
             (sut/sub p v)))))

  (testing "Subtracting two vectors"
    (let [v1 (sut/vector 3 2 1)
          v2 (sut/vector 5 6 7)]
      (is (= (sut/vector -2 -4 -6 )
             (sut/sub v1 v2)))))

  (testing "Subtracting a vector from the zero vector"
    (let [zero (sut/vector 0 0 0 )
          v (sut/vector 1 -2 3)]
      (is (= (sut/vector -1 2 -3)
             (sut/sub zero v))))))

(deftest negating-a-tuple
  (let [t (#'sut/tuple 1 -2 3 -4)]
    (is (= (#'sut/tuple -1 2 -3 4)
           (sut/negate t)))))

(deftest multiplying-tuples
  (testing "Multiplying a tuple by a scaler"
    (let [t (#'sut/tuple 1 -2 3 -4)]
      (is (= (#'sut/tuple 3.5 -7.0 10.5 -14.0)
             (sut/mul t 3.5)))))

  (testing "Multiplying a tuple by a fraction"
    (let [t (#'sut/tuple 1 -2 3 -4)]
      (is (= (#'sut/tuple 0.5 -1.0 1.5 -2.0)
             (sut/mul t 0.5))))))

(deftest dividing-tuples
  (testing "Dividing a tuple by a scalar"
    (let [t (#'sut/tuple 1 -2 3 -4)]
      (is (= (#'sut/tuple 0.5 -1.0 1.5 -2.0)
             (sut/div t 2))))))

(deftest magnitude
  (is (= 1.0 (sut/magnitude (sut/vector 1 0 0))))
  (is (= 1.0 (sut/magnitude (sut/vector 0 1 0))))
  (is (= 1.0 (sut/magnitude (sut/vector 0 0 1))))

  (is (= (Math/sqrt 14.0) (sut/magnitude (sut/vector 1 2 3))))
  (is (= (Math/sqrt 14.0) (sut/magnitude (sut/vector -1 -2 -3)))))

(deftest normalising-tuples
  (is (= (sut/vector 1 0 0)
         (sut/normalise (sut/vector 4 0 0))))
  (is (approx (sut/vector 0.26726 0.53452 0.80178)
              (sut/normalise (sut/vector 1 2 3)))))

(deftest the-magnitude-of-a-normalised-vector
  (let [v (sut/vector 1 2 3)
        norm (sut/normalise v)]
    (is (= 1.0 (sut/magnitude norm)))))
