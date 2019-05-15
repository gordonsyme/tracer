(ns tracer.entities.world-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.intersection :as i]
            [tracer.entities.material :as material]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.ray :as ray]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.entities.world :as world]))

(clojure.test/use-fixtures :once instrument)

(defn- default-world
  []
  (-> (world/world)
      (world/add-light (light/point-light
                         (tup/point -10 10 -10)
                         (colour/colour 1 1 1)))
      (world/add-object (sphere/with-material
                          (sphere/sphere)
                          (-> (material/material)
                              (material/with-colour (colour/colour 0.8 1.0 0.6))
                              (material/with-diffuse 0.7)
                              (material/with-specular 0.2))))
      (world/add-object (sphere/with-transform (sphere/sphere)
                          (transform/scaling 0.5 0.5 0.5)))))

(deftest creating-a-world
  (let [w (world/world)]
    (is (empty? (world/objects w)))
    (is (empty? (world/lights w)))))

(deftest the-default-world
  (let [light (light/point-light (tup/point -10 10 -10)
                                 (colour/colour 1 1 1))
        s1 (sphere/with-material (sphere/sphere)
                                 (-> (material/material)
                                     (material/with-colour (colour/colour 0.8 1.0 0.6))
                                     (material/with-diffuse 0.7)
                                     (material/with-specular 0.2)))
        s2 (sphere/with-transform (sphere/sphere)
                                  (transform/scaling 0.5 0.5 0.5))
        w (default-world)]
    (is (= [light] (world/lights w)))
    (is (contains? (set (world/objects w)) s1))
    (is (contains? (set (world/objects w)) s2))))

(deftest intersect-a-world-with-a-ray
  (let [w (default-world)
        r (ray/ray (tup/point 0 0 -5) (tup/vector 0 0 1))]
    (is (= [4.0 4.5 5.5 6.0]
           (map :t (world/intersect w r))))))

(deftest shading-an-intersection
  (let [w (default-world)
        shape (first (world/objects w))
        comps (i/prepare-computations (i/intersection 4 shape)
                                      (ray/ray (tup/point 0 0 -5)
                                               (tup/vector 0 0 1)))]
    (is (approx (colour/colour 0.38066 0.47583 0.2855)
                (world/shade-hit w comps)))))

(deftest shading-an-intersection-from-the-inside
  (let [w (assoc (default-world)
                 :lights [(light/point-light (tup/point 0 0.25 0)
                                             (colour/colour 1 1 1))])
        shape (second (world/objects w))
        comps (i/prepare-computations (i/intersection 0.5 shape)
                                      (ray/ray (tup/point 0 0 0)
                                               (tup/vector 0 0 1)))]
    (is (approx (colour/colour 0.90498 0.90498 0.90498)
                (world/shade-hit w comps)))))
