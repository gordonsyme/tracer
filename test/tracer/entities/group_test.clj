(ns tracer.entities.group-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.cube :as cube]
            [tracer.entities.group :as group]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest creating-a-new-group
  (let [g (group/group)]
    (is (= (transform/identity)
           (:transform g)))
    (is (empty? (group/children (shape/relations) g)))))

(deftest adding-a-child-to-a-group
  (let [s (sphere/sphere)
        g (group/group)
        rels (group/add-child (shape/relations) g s)]
    (is (= [s] (group/children rels g)))
    (is (= g (shape/parent rels s)))))

(deftest adding-many-children-to-a-group
  (let [s1 (sphere/sphere)
        s2 (sphere/sphere)
        g (group/group)
        rels (group/add-children (shape/relations) g [s1 s2])]
    (is (= [s1 s2] (group/children rels g)))
    (is (= g (shape/parent rels s1)))
    (is (= g (shape/parent rels s2)))))

(deftest intersecting-a-ray-with-an-empty-group
  (let [g (group/group)
        r (ray/ray (tup/point 0 0 0) (tup/vector 0 0 1))]
    (is (empty? (shape/intersect (shape/relations) g r)))))

(deftest intersecting-a-ray-with-a-non-empty-group
  (let [g (group/group)
        s1 (sphere/sphere)
        s2 (shape/with-transform (sphere/sphere)
                                 (transform/translation 0 0 -3))
        s3 (shape/with-transform (sphere/sphere)
                                 (transform/translation 5 0 0))
        rels (-> (shape/relations)
                 (group/add-child g s1)
                 (group/add-child g s2)
                 (group/add-child g s3))
        r (ray/ray (tup/point 0 0 -5) (tup/vector 0 0 1))]
    (is (= [s2 s2 s1 s1]
           (map :object (shape/intersect rels g r))))))

(deftest intersecting-a-transformed-group
  (let [rels (shape/relations)
        g (shape/with-transform (group/group)
                                (transform/scaling 2 2 2))
        s (shape/with-transform (sphere/sphere)
                                (transform/translation 5 0 0))
        rels (group/add-child rels g s)]
    (is (= 2
           (count (shape/intersect rels g (ray/ray (tup/point 10 0 -10)
                                                   (tup/vector 0 0 1))))))))

(deftest empty-group-has-empty-bounds
  (let [g (group/group)]
    (is (= {:min-bound (tup/point 0 0 0)
            :max-bound (tup/point 0 0 0)}
           (shape/bounds (shape/relations) g)))))

(deftest a-groups-bounding-box-contains-all-the-groups-children
  (testing "a single axis-aligned unit cube"
    (let [g (group/group)
          c (cube/cube)
          rels (group/add-child (shape/relations) g c)]
      (is (= (shape/bounds rels c)
             (shape/bounds rels g)))))

  (testing "a single unit sphere"
    (let [g (group/group)
          s (sphere/sphere)
          rels (group/add-child (shape/relations) g s)]
      (is (= {:min-bound (tup/point -1 -1 -1)
              :max-bound (tup/point 1 1 1)}
             (shape/bounds rels g)))))

  (testing "a cube rotated around 2 axes"
    (let [root-2 (Math/sqrt 2)
          g (group/group)
          c (shape/with-transform
              (cube/cube)
              (-> (transform/identity)
                  (transform/rotate-x (/ Math/PI 4))
                  (transform/rotate-y (/ Math/PI 4))))
          rels (group/add-child (shape/relations) g c)
          {:keys [min-bound max-bound]} (shape/bounds rels g)]
      (is (approx (tup/point -1.707106 (- root-2) -1.707106)
                  min-bound))
      (is (approx (tup/point 1.707106 root-2 1.707106)
                  max-bound))))

  (testing "bounds extend to cover multiple shapes"
    (let [g (group/group)
          s1 (shape/with-transform
               (sphere/sphere)
               (transform/translation -5 0 0))
          s2 (shape/with-transform
               (sphere/sphere)
               (transform/translation 0 2 0))
          rels (group/add-children (shape/relations) g [s1 s2])
          {:keys [min-bound max-bound]} (shape/bounds rels g)]
      (is (approx (tup/point -6 -1 -1)
                  min-bound))
      (is (approx (tup/point 1 3 1)
                  max-bound)))))
