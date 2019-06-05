(ns tracer.entities.world-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.test-entities :refer (default-world)]
            [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.intersection :as i]
            [tracer.entities.material :as material]
            [tracer.entities.shape :as shape]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.ray :as ray]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.entities.world :as world]))

(clojure.test/use-fixtures :once instrument)

(deftest creating-a-world
  (let [w (world/world)]
    (is (empty? (world/objects w)))
    (is (empty? (world/lights w)))))

(deftest the-default-world
  (let [light (light/point-light (tup/point -10 10 -10)
                                 (colour/colour 1 1 1))
        s1 (shape/with-material (sphere/sphere)
                                (-> (material/material)
                                    (material/with-colour (colour/colour 0.8 1.0 0.6))
                                    (material/with-diffuse 0.7)
                                    (material/with-specular 0.2)))
        s2 (shape/with-transform (sphere/sphere)
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

(deftest shading-an-intersection-in-shadow
  (let [s1 (sphere/sphere)
        s2 (shape/with-transform
             (sphere/sphere)
             (transform/translation 0 0 10))
        w (-> (default-world)
              (assoc :lights [(light/point-light (tup/point 0 0 -10)
                                                 (colour/colour 1 1 1))])
              (world/add-object s1)
              (world/add-object s2))
        r (ray/ray (tup/point 0 0 5)
                   (tup/vector 0 0 1))
        comps (i/prepare-computations (i/intersection 4 s2) r)]
    (is (= (colour/colour 0.1 0.1 0.1)
           (world/shade-hit w comps)))))

(deftest colouring-hits-in-the-world
  (let [w (default-world)]
    (testing "when the ray misses"
      (let [r (ray/ray (tup/point 0 0 -5) (tup/vector 0 1 0))]
        (is (= (colour/colour 0 0 0)
               (world/colour-at w r)))))

    (testing "when the ray hits"
      (let [r (ray/ray (tup/point 0 0 -5) (tup/vector 0 0 1))]
        (is (approx (colour/colour 0.38066 0.47583 0.2855)
                    (world/colour-at w r)))))

    (testing "when there is an intersection behind the ray"
      (let [w (update w :objects (fn [objs]
                                   (map (fn [o]
                                          (update o :material material/with-ambient 1))
                                        objs)))
            inner (second (world/objects w))
            r (ray/ray (tup/point 0 0 0.75) (tup/vector 0 0 -1))]
        (is (= (-> inner :material :colour)
               (world/colour-at w r)))))))

(deftest shadowing
  (let [w (default-world)
        l (first (world/lights w))]
    (testing "no shadow when nothing is colinear with the point and light"
      (is (false? (world/shadowed? w l (tup/point 0 10 0)))))

    (testing "shadowed when an object is between the point and the light"
      (is (true? (world/shadowed? w l (tup/point 10 -10 10)))))

    (testing "there is no shadow when an object is behind the light"
      (is (false? (world/shadowed? w l (tup/point -20 20 -20)))))

    (testing "no shadow when an object is behind the point"
      (is (false? (world/shadowed? w l (tup/point -2 2 -2)))))))
