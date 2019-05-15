(ns tracer.entities.sphere-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.intersection :as i]
            [tracer.entities.material :as material]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as ray]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest a-ray-intersects-a-sphere-at-two-points
  (testing "two distinct points"
    (let [r (ray/ray (tup/point 0 0 -5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t 4.0 :object s}
              {:t 6.0 :object s}]
             (i/intersect s r)))))

  (testing "a tangent"
    (let [r (ray/ray (tup/point 0 1 -5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t 5.0 :object s} {:t 5.0 :object s}]
             (i/intersect s r)))))

  (testing "the ray misses"
    (let [r (ray/ray (tup/point 0 2 -5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= []
             (i/intersect s r)))))

  (testing "the ray originates inside the sphere"
    (let [r (ray/ray (tup/point 0 0 0)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t -1.0 :object s} {:t 1.0 :object s}]
             (i/intersect s r)))))

  (testing "the sphere is behind the ray"
    (let [r (ray/ray (tup/point 0 0 5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t -6.0 :object s} {:t -4.0 :object s}]
             (i/intersect s r))))))

(deftest sphere-transforms
  (let [s (sphere/sphere)]
    (testing "default transform"
      (is (= (mat/identity 4)
             (:transform s))))
    (testing "setting a transform"
      (let [tr (transform/translation 1 2 3)
            s (sphere/with-transform s tr)]
        (is (= tr (:transform s)))))))

(deftest intersecting-a-scaled-sphere-with-a-ray
  (let [r (ray/ray (tup/point 0 0 -5)
                   (tup/vector 0 0 1))
        s (sphere/with-transform (sphere/sphere)
                                 (transform/scaling 2 2 2))]
    (is (= [{:t 3.0 :object s} {:t 7.0 :object s}]
           (i/intersect s r)))))

(deftest intersecting-a-translated-sphere-with-a-ray
  (let [r (ray/ray (tup/point 0 0 -5)
                   (tup/vector 0 0 1))
        s (sphere/with-transform (sphere/sphere)
                                 (transform/translation 5 0 0))]
    (is (= [] (i/intersect s r)))))

(deftest surface-normals
  (let [s (sphere/sphere)]
    (testing "the normal on a sphere at a point on the x axis"
      (is (= (tup/vector 1 0 0)
             (i/normal-at s (tup/point 1 0 0)))))
    (testing "the normal on a sphere at a point on the y axis"
      (is (= (tup/vector 0 1 0)
             (i/normal-at s (tup/point 0 1 0)))))
    (testing "the normal on a sphere at a point on the z axis"
      (is (= (tup/vector 0 0 1)
             (i/normal-at s (tup/point 0 0 1)))))
    (testing "the normal on a sphere at a nonaxial point"
      (let [v (/ (Math/sqrt 3) 3)]
        (is (= (tup/vector v v v)
               (i/normal-at s (tup/point v v v))))))

    (testing "normals are normalised"
      (let [v (/ (Math/sqrt 3) 3)
            n (i/normal-at s (tup/point v v v))]
        (is (= n (tup/normalise n)))))))

(deftest normals-on-transformed-spheres
  (testing "normal on a translated sphere"
    (let [s (sphere/with-transform (sphere/sphere)
                                   (transform/translation 0 1 0))]
      (is (approx (tup/vector 0 0.70711 -0.70711)
                  (i/normal-at s (tup/point 0 1.70711 -0.70711))))))

  (testing "normal on a transformed sphere"
    (let [s (sphere/with-transform (sphere/sphere)
                                   (-> (transform/rotation-z (/ Math/PI 5))
                                       (transform/scale 1 0.5 1)))
          root-2-over-2 (/ (Math/sqrt 2) 2)]
      (is (approx (tup/vector 0 0.97014 -0.24254)
                  (i/normal-at s (tup/point 0 root-2-over-2 (- root-2-over-2))))))))

(deftest a-sphere-has-a-default-material
  (let [s (sphere/sphere)
        m (material/material)]
    (is (= m (:material s)))))

(deftest a-sphere-may-be-assigned-a-material
  (let [m (assoc (material/material)
                 :ambient 1.0)
        s (sphere/with-material (sphere/sphere) m)]
    (is (= m (:material s)))))
