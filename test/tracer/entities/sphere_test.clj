(ns tracer.entities.sphere-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.intersection :as i]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
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
             (i/intersect (shape/relations) s r)))))

  (testing "a tangent"
    (let [r (ray/ray (tup/point 0 1 -5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t 5.0 :object s} {:t 5.0 :object s}]
             (i/intersect (shape/relations) s r)))))

  (testing "the ray misses"
    (let [r (ray/ray (tup/point 0 2 -5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= []
             (i/intersect (shape/relations) s r)))))

  (testing "the ray originates inside the sphere"
    (let [r (ray/ray (tup/point 0 0 0)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t -1.0 :object s} {:t 1.0 :object s}]
             (i/intersect (shape/relations) s r)))))

  (testing "the sphere is behind the ray"
    (let [r (ray/ray (tup/point 0 0 5)
                     (tup/vector 0 0 1))
          s (sphere/sphere)]
      (is (= [{:t -6.0 :object s} {:t -4.0 :object s}]
             (i/intersect (shape/relations) s r))))))

(deftest intersecting-a-scaled-sphere-with-a-ray
  (let [r (ray/ray (tup/point 0 0 -5)
                   (tup/vector 0 0 1))
        s (shape/with-transform (sphere/sphere)
                                (transform/scaling 2 2 2))]
    (is (= [{:t 3.0 :object s} {:t 7.0 :object s}]
           (i/intersect (shape/relations) s r)))))

(deftest intersecting-a-translated-sphere-with-a-ray
  (let [r (ray/ray (tup/point 0 0 -5)
                   (tup/vector 0 0 1))
        s (shape/with-transform (sphere/sphere)
                                (transform/translation 5 0 0))]
    (is (= [] (i/intersect (shape/relations) s r)))))

(deftest surface-normals
  (let [s (sphere/sphere)]
    (testing "the normal on a sphere at a point on the x axis"
      (is (= (tup/vector 1 0 0)
             (shape/normal-at (shape/relations) s (tup/point 1 0 0)))))
    (testing "the normal on a sphere at a point on the y axis"
      (is (= (tup/vector 0 1 0)
             (shape/normal-at (shape/relations) s (tup/point 0 1 0)))))
    (testing "the normal on a sphere at a point on the z axis"
      (is (= (tup/vector 0 0 1)
             (shape/normal-at (shape/relations) s (tup/point 0 0 1)))))
    (testing "the normal on a sphere at a nonaxial point"
      (let [v (/ (Math/sqrt 3) 3)]
        (is (= (tup/vector v v v)
               (shape/normal-at (shape/relations) s (tup/point v v v))))))

    (testing "normals are normalised"
      (let [v (/ (Math/sqrt 3) 3)
            n (shape/normal-at (shape/relations) s (tup/point v v v))]
        (is (= n (tup/normalise n)))))))

(deftest normals-on-transformed-spheres
  (testing "normal on a translated sphere"
    (let [s (shape/with-transform (sphere/sphere)
                                  (transform/translation 0 1 0))]
      (is (approx (tup/vector 0 0.70711 -0.70711)
                  (shape/normal-at (shape/relations) s (tup/point 0 1.70711 -0.70711))))))

  (testing "normal on a transformed sphere"
    (let [s (shape/with-transform (sphere/sphere)
                                  (-> (transform/rotation-z (/ Math/PI 5))
                                      (transform/scale 1 0.5 1)))
          root-2-over-2 (/ (Math/sqrt 2) 2)]
      (is (approx (tup/vector 0 0.97014 -0.24254)
                  (shape/normal-at (shape/relations) s (tup/point 0 root-2-over-2 (- root-2-over-2))))))))
