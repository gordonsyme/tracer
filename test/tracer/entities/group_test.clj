(ns tracer.entities.group-test
  (:require [clojure.test :refer (deftest is)]
            [tracer.fixtures :refer (instrument)]
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
