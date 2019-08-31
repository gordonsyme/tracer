(ns tracer.entities.transform-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx eq)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.matrix :as mat]
            [tracer.entities.tuple :as tup]
            [tracer.entities.transform :as t]))

(clojure.test/use-fixtures :once instrument)

(deftest multiplying-by-a-translation-matrix
  (let [transform (t/translation 5 -3 2)
        p (tup/point -3 4 5)]
    (testing "multiplying by a point"
      (is (eq (tup/point 2 1 7)
              (t/apply transform p))))

    (testing "multiplying the inverse translation by a point"
      (is (eq (tup/point -8 7 3)
              (t/apply (mat/inverse transform) p))))

    (testing "multiplying a translation by a vector is a noop"
      (let [v (tup/vector -3 4 5)]
        (is (eq v (t/apply transform v)))))))

(deftest multiplying-by-a-scaling-matrix
  (let [transform (t/scaling 2 3 4)]
    (testing "multiplied by a point"
      (is (eq (tup/point -8 18 32)
              (t/apply transform
                       (tup/point -4 6 8)))))

    (testing "multiplying by a vector"
      (is (eq (tup/vector -8 18 32)
              (t/apply transform
                       (tup/vector -4 6 8)))))

    (testing "multiplying the inverse by a vector"
      (is (eq (tup/vector -2 2 2)
              (t/apply (mat/inverse transform)
                       (tup/vector -4 6 8)))))))

(deftest reflection-is-scaling-by-a-negative-value
  (let [transform (t/scaling -1 1 1)
        p (tup/point 2 3 4)]
    (is (eq (tup/point -2 3 4)
            (t/apply transform p)))))

(deftest rotating-a-point-around-the-x-axis
  (let [p (tup/point 0 1 0)
        half-quarter (t/rotation-x (/ Math/PI 4))
        full-quarter (t/rotation-x (/ Math/PI 2))]
    (testing "rotation"
      (is (approx (tup/point 0
                             (/ (Math/sqrt 2) 2)
                             (/ (Math/sqrt 2) 2))
                  (t/apply half-quarter p)))
      (is (approx (tup/point 0 0 1)
                  (t/apply full-quarter p))))

    (testing "rotation by the inverse rotates the other way"
      (let [inv (mat/inverse half-quarter)]
        (is (approx (tup/point 0
                               (/ (Math/sqrt 2) 2)
                               (- (/ (Math/sqrt 2) 2)))
                    (t/apply inv p)))))

    (testing "fluent API"
      (is (approx (tup/point 0
                             (/ (Math/sqrt 2) 2)
                             (/ (Math/sqrt 2) 2))
                  (t/apply (t/rotate-x (t/identity)
                                        (/ Math/PI 4))
                           p))))))

(deftest rotating-a-point-around-the-y-axis
  (let [p (tup/point 0 0 1)
        half-quarter (t/rotation-y (/ Math/PI 4))
        full-quarter (t/rotation-y (/ Math/PI 2))]
    (testing "rotation"
      (is (approx (tup/point (/ (Math/sqrt 2) 2)
                             0
                             (/ (Math/sqrt 2) 2))
                  (t/apply half-quarter p)))
      (is (approx (tup/point 1 0 0)
                  (t/apply full-quarter p))))

    (testing "rotation by the inverse rotates the other way"
      (let [inv (mat/inverse half-quarter)]
        (is (approx (tup/point (- (/ (Math/sqrt 2) 2))
                               0
                               (/ (Math/sqrt 2) 2))
                    (t/apply inv p)))))

    (testing "fluent API"
      (is (approx (tup/point (/ (Math/sqrt 2) 2)
                             0
                             (/ (Math/sqrt 2) 2))
                  (t/apply (t/rotate-y (t/identity)
                                        (/ Math/PI 4))
                           p))))))

(deftest rotating-a-point-around-the-z-axis
  (let [p (tup/point 0 1 0)
        half-quarter (t/rotation-z (/ Math/PI 4))
        full-quarter (t/rotation-z (/ Math/PI 2))]
    (testing "rotation"
      (is (approx (tup/point (- (/ (Math/sqrt 2) 2))
                             (/ (Math/sqrt 2) 2)
                             0)
                  (t/apply half-quarter p)))
      (is (approx (tup/point -1 0 0)
                  (t/apply full-quarter p))))

    (testing "rotation by the inverse rotates the other way"
      (let [inv (mat/inverse half-quarter)]
        (is (approx (tup/point (/ (Math/sqrt 2) 2)
                               (/ (Math/sqrt 2) 2)
                               0)
                    (t/apply inv p)))))

    (testing "fluent API"
      (is (approx (tup/point (- (/ (Math/sqrt 2) 2))
                             (/ (Math/sqrt 2) 2)
                             0)
                  (t/apply (t/rotate-z (t/identity)
                                        (/ Math/PI 4))
                           p))))))

(deftest shearing
  (let [p (tup/point 2 3 4)
        ident (t/identity)]
    (testing "x in proportion to y"
      (is (eq (tup/point 5 3 4)
              (t/apply (t/shearing :xy 1) p)
              (t/apply (t/shear ident :xy 1) p)
              (t/apply (#'t/shearing* 1 0 0 0 0 0) p))))

    (testing "x in proportion to z"
      (is (eq (tup/point 6 3 4)
              (t/apply (t/shearing :xz 1) p)
              (t/apply (t/shear ident :xz 1) p)
              (t/apply (#'t/shearing* 0 1 0 0 0 0) p))))

    (testing "y in proportion to x"
      (is (eq (tup/point 2 5 4)
              (t/apply (t/shearing :yx 1) p)
              (t/apply (t/shear ident :yx 1) p)
              (t/apply (#'t/shearing* 0 0 1 0 0 0) p))))

    (testing "y in proportion to z"
      (is (eq (tup/point 2 7 4)
              (t/apply (t/shearing :yz 1) p)
              (t/apply (t/shear ident :yz 1) p)
              (t/apply (#'t/shearing* 0 0 0 1 0 0) p))))

    (testing "z in proportion to x"
      (is (eq (tup/point 2 3 6)
              (t/apply (t/shearing :zx 1) p)
              (t/apply (t/shear ident :zx 1) p)
              (t/apply (#'t/shearing* 0 0 0 0 1 0) p))))

    (testing "z in proportion to y"
      (is (eq (tup/point 2 3 7)
              (t/apply (t/shearing :zy 1) p)
              (t/apply (t/shear ident :zy 1) p)
              (t/apply (#'t/shearing* 0 0 0 0 0 1) p))))))

(deftest individual-transformations-are-applied-in-sequence
  (let [p (tup/point 1 0 1)
        a (t/rotation-x (/ Math/PI 2))
        b (t/scaling 5 5 5)
        c (t/translation 10 5 7)
        p2 (t/apply a p)
        p3 (t/apply b p2)
        p4 (t/apply c p3)]
    (is (approx (tup/point 1 -1 0) p2))
    (is (approx (tup/point 5 -5 0) p3))
    (is (approx (tup/point 15 0 7) p4))))

(deftest chained-transformations-are-applied-in-reverse-order
  (let [p (tup/point 1 0 1)
        a (t/rotation-x (/ Math/PI 2))
        b (t/scaling 5 5 5)
        c (t/translation 10 5 7)
        chained (->> a
                     (mat/mul b)
                     (mat/mul c))]
    (is (eq (tup/point 15 0 7)
            (t/apply chained p)))))

(deftest fluent-api-works
  (let [transform (-> (t/identity)
                      (t/rotate-x (/ Math/PI 2))
                      (t/scale 5 5 5)
                      (t/translate 10 5 7))]
    (is (eq (tup/point 15 0 7)
            (t/apply transform (tup/point 1 0 1))))))

(deftest view-transformations
  (testing "the transformation matrix for the default orientation"
    (let [from (tup/point 0 0 0)
          to (tup/point 0 0 -1)
          up (tup/vector 0 1 0)]
      (is (= (t/identity)
             (t/view-transform from to up)))))

  (testing "the transformation matrix looking in the positive z direction"
    (let [from (tup/point 0 0 0)
          to (tup/point 0 0 1)
          up (tup/vector 0 1 0)]
      (is (= (t/scaling -1 1 -1)
             (t/view-transform from to up)))))

  (testing "the view transformation moves the world"
    (let [from (tup/point 0 0 8)
          to (tup/point 0 0 0)
          up (tup/vector 0 1 0)]
      (is (= (t/translation 0 0 -8)
             (t/view-transform from to up)))))

  (testing "an arbitrary view transformation"
    (let [from (tup/point 1 3 2)
          to (tup/point 4 -2 8)
          up (tup/vector 1 1 0)]
      (is (approx (mat/matrix [-0.50709 0.50709  0.67612 -2.36643]
                              [ 0.76772 0.60609  0.12122 -2.82843]
                              [-0.35857 0.59761 -0.71714  0.0]
                              [ 0.0     0.0      0.0      1.0])
                  (t/view-transform from to up))))))
