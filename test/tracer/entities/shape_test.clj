(ns tracer.entities.shape-test
  (:require [clojure.test :refer (deftest testing is)]
            [clojure.spec.alpha :as s]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.group :as group]
            [tracer.entities.material :as material]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.sphere :as sphere]
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
  [_rels s ray]
  (swap! (:capture-atom s) assoc ::shape/local-intersect ray)
  [])

(defmethod shape/local-normal-at :test-shape
  [_s p]
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
      (shape/intersect (shape/relations) s r)
      (is (= (ray/ray (tup/point 0 0 -2.5)
                      (tup/vector 0 0 0.5))
             (::shape/local-intersect @capture)))))

  (testing "intersecting a translated shape with a ray"
    (let [r (ray/ray (tup/point 0 0 -5)
                     (tup/vector 0 0 1))
          capture (atom {})
          s (shape/with-transform (test-shape capture)
                                  (transform/translation 5 0 0))]
      (shape/intersect (shape/relations) s r)
      (is (= (ray/ray (tup/point -5 0 -5)
                      (tup/vector 0 0 1))
             (::shape/local-intersect @capture))))))

(deftest shape-normals
  (testing "computing the normal on a translated shape"
    (let [s (shape/with-transform (test-shape)
                                  (transform/translation 0 1 0))]
      (is (approx (tup/vector 0 0.70711 -0.70711)
                  (shape/normal-at (shape/relations) s (tup/point 0 1.70711 -0.70711))))))

  (testing "computing the normal on a transformed shape"
    (let [root-2-over-2 (/ (Math/sqrt 2) 2)
          s (shape/with-transform
              (test-shape)
              (-> (transform/identity)
                  (transform/rotate-z (/ Math/PI 5))
                  (transform/scale 1 0.5 1)))]
      (is (approx (tup/vector 0 0.97014 -0.24254)
                  (shape/normal-at (shape/relations) s (tup/point 0 root-2-over-2 (- root-2-over-2))))))))

(deftest a-shape-has-a-parent-attribute
  (let [s (sphere/sphere)
        rels (shape/relations)]
    (is (nil? (shape/parent rels s)))))

(deftest converting-a-point-from-world-to-object-space-with-groups
  (let [g1 (shape/with-transform
             (group/group)
             (transform/rotation-y (/ Math/PI 2)))
        g2 (shape/with-transform
             (group/group)
             (transform/scaling 2 2 2))
        s (shape/with-transform
            (sphere/sphere)
            (transform/translation 5 0 0))
        rels (-> (shape/relations)
                 (group/add-child g1 g2)
                 (group/add-child g2 s))]
    (is (approx (tup/point 0 0 -1)
                (shape/world-to-object rels s (tup/point -2 0 -10))))))

(deftest converting-a-normal-from-object-to-world-space
  (let [root-3-over-3 (/ (Math/sqrt 3) 3)
        g1 (shape/with-transform
             (group/group)
             (transform/rotation-y (/ Math/PI 2)))
        g2 (shape/with-transform
             (group/group)
             (transform/scaling 1 2 3))
        s (shape/with-transform
            (sphere/sphere)
            (transform/translation 5 0 0))
        rels (-> (shape/relations)
                 (group/add-child g1 g2)
                 (group/add-child g2 s))]
    (is (approx (tup/vector 0.28571 0.42857 -0.85714)
                (shape/normal-to-world rels s (tup/vector root-3-over-3
                                                          root-3-over-3
                                                          root-3-over-3))))))

(deftest finding-the-normal-on-a-child-object
  (let [g1 (shape/with-transform
             (group/group)
             (transform/rotation-y (/ Math/PI 2)))
        g2 (shape/with-transform
             (group/group)
             (transform/scaling 1 2 3))
        s (shape/with-transform
            (sphere/sphere)
            (transform/translation 5 0 0))
        rels (-> (shape/relations)
                 (group/add-child g1 g2)
                 (group/add-child g2 s))]
    (is (approx (tup/vector 0.28570 0.42854 -0.85716)
                (shape/normal-at rels s (tup/point 1.7321 1.1547 -5.5774))))))
