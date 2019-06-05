(ns tracer.entities.shape-test
  (:require [clojure.test :refer (deftest testing is)]
            [clojure.spec.alpha :as s]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.material :as material]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(s/def ::test-shape (s/and ::shape/common
                           #(= :test-shape (::shape/tag %))))
(defmethod shape/object-type :test-shape
  [_]
  ::test-shape)

(defn- test-shape
  ([] (test-shape (atom {})))
  ([capture]
   {::shape/tag :test-shape
    :transform (transform/identity)
    :inverse-transform (transform/identity)
    :material (material/material)
    :capture-atom capture}))
(s/fdef test-shape
  :ret ::test-shape)

(defmethod shape/local-intersect :test-shape
  [s ray]
  (swap! (:capture-atom s) assoc ::shape/local-intersect ray)
  [])

(defmethod shape/local-normal-at :test-shape
  [s p]
  (tup/vector (tup/x p) (tup/y p) (tup/z p)))

(deftest shape-transformations
  (testing "the default transform"
    (let [s (test-shape)]
      (is (= (transform/identity)
             (shape/transform s)))))

  (testing "assigning a transform"
    (let [s (shape/with-transform (test-shape)
                                  (transform/translation 2 3 4))]
      (is (= (transform/translation 2 3 4)
             (shape/transform s))))))

(deftest shape-materials
  (testing "the default material"
    (let [s (test-shape)]
      (is (= (material/material)
             (shape/material s)))))

  (testing "assigning a material"
    (let [m (material/with-ambient (material/material) 1)
          s (shape/with-material (test-shape) m)]
      (is (= m (shape/material s))))))

(deftest shape-intersections
  (testing "intersecting a scaled shape with a ray"
    (let [r (ray/ray (tup/point 0 0 -5)
                     (tup/vector 0 0 1))
          capture (atom {})
          s (shape/with-transform (test-shape capture)
                                  (transform/scaling 2 2 2))]
      (shape/intersect s r)
      (is (= (ray/ray (tup/point 0 0 -2.5)
                      (tup/vector 0 0 0.5))
             (::shape/local-intersect @capture)))))

  (testing "intersecting a translated shape with a ray"
    (let [r (ray/ray (tup/point 0 0 -5)
                     (tup/vector 0 0 1))
          capture (atom {})
          s (shape/with-transform (test-shape capture)
                                  (transform/translation 5 0 0))]
      (shape/intersect s r)
      (is (= (ray/ray (tup/point -5 0 -5)
                      (tup/vector 0 0 1))
             (::shape/local-intersect @capture))))))

(deftest shape-normals
  (testing "computing the normal on a translated shape"
    (let [s (shape/with-transform (test-shape)
                                  (transform/translation 0 1 0))]
      (is (approx (tup/vector 0 0.70711 -0.70711)
                  (shape/normal-at s (tup/point 0 1.70711 -0.70711))))))

  (testing "computing the normal on a transformed shape"
    (let [root-2-over-2 (/ (Math/sqrt 2) 2)
          s (shape/with-transform
              (test-shape)
              (-> (transform/identity)
                  (transform/rotate-z (/ Math/PI 5))
                  (transform/scale 1 0.5 1)))]
      (is (approx (tup/vector 0 0.97014 -0.24254)
                  (shape/normal-at s (tup/point 0 root-2-over-2 (- root-2-over-2))))))))
