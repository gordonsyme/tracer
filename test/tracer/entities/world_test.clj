(ns tracer.entities.world-test
  (:require [clojure.test :refer (deftest testing is)]
            [clojure.spec.alpha :as s]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.test-entities :refer (default-world)]
            [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.intersection :as i]
            [tracer.entities.material :as material]
            [tracer.entities.pattern :as pattern]
            [tracer.entities.plane :as plane]
            [tracer.entities.shape :as shape]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.ray :as ray]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.entities.world :as world]))

(clojure.test/use-fixtures :once instrument)

(defn- test-pattern
  []
  {:transform (transform/identity)
   :inverse-transform (transform/identity)
   :shader (fn [point]
             (colour/colour (tup/x point) (tup/y point) (tup/z point)))})
(s/fdef test-pattern
  :ret ::pattern/pattern)

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
        comps (i/prepare-computations (world/relations w) (i/intersection 4 shape)
                                      (ray/ray (tup/point 0 0 -5)
                                               (tup/vector 0 0 1)))]
    (is (approx (colour/colour 0.38066 0.47583 0.2855)
                (#'world/shade-hit w comps)))))

(deftest shading-an-intersection-from-the-inside
  (let [w (assoc (default-world)
                 :lights [(light/point-light (tup/point 0 0.25 0)
                                             (colour/colour 1 1 1))])
        shape (second (world/objects w))
        comps (i/prepare-computations (world/relations w) (i/intersection 0.5 shape)
                                      (ray/ray (tup/point 0 0 0)
                                               (tup/vector 0 0 1)))]
    (is (approx (colour/colour 0.90498 0.90498 0.90498)
                (#'world/shade-hit w comps)))))

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
        comps (i/prepare-computations (world/relations w) (i/intersection 4 s2) r)]
    (is (= (colour/colour 0.1 0.1 0.1)
           (#'world/shade-hit w comps)))))

(deftest shading-an-intersection-with-a-pattern
  (let [s (-> (sphere/sphere)
              (shape/with-transform
                (transform/translation 0 0 10))
              (shape/with-material
                (-> (material/material)
                    (material/with-ambient 1)
                    (material/with-specular 0)
                    (material/with-diffuse 0)
                    (material/with-pattern
                      (pattern/stripes (colour/colour 0.2 0.2 0.2)
                                       (colour/colour 0.8 0.8 0.8))))))
        w (-> (default-world)
              (assoc :lights [(light/point-light (tup/point 0 0 -10)
                                                 (colour/colour 1 1 1))])
              (world/add-object s))
        r (ray/ray (tup/point 0 0 5)
                   (tup/vector 0 0 1))
        comps (i/prepare-computations (world/relations w) (i/intersection 4 s) r)]
    (is (approx (colour/colour 0.2 0.2 0.2)
                (#'world/shade-hit w comps)))))

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

(deftest reflections
  (let [root-2-over-2 (/ (Math/sqrt 2) 2)
        w (default-world)]
    (testing "the reflected colour for a nonreflective material"
      (let [r (ray/ray (tup/point 0 0 0) (tup/vector 0 0 1))
            shape (let [s (second (world/objects w))]
                    (shape/with-material s
                      (material/with-ambient (shape/material s) 1)))
            comps (i/prepare-computations (world/relations w) (i/intersection 1 shape) r)]
        (is (= (colour/colour 0 0 0)
               (world/reflected-colour w comps)))))

    (testing "the reflected colour for a reflective material"
      (let [shape (-> (plane/plane)
                      (shape/with-material
                        (-> (material/material)
                            (material/with-reflective 0.5)))
                      (shape/with-transform
                        (transform/translation 0 -1 0)))
            w (world/add-object w shape)
            r (ray/ray (tup/point 0 0 -3)
                       (tup/vector 0 (- root-2-over-2) root-2-over-2))
            comps (i/prepare-computations (world/relations w) (i/intersection (Math/sqrt 2) shape) r)]
        (is (approx (colour/colour 0.19033 0.23791 0.14274)
                    (world/reflected-colour w comps)))))

    (testing "shading hits with reflective materials"
      (let [shape (-> (plane/plane)
                      (shape/with-material
                        (-> (material/material)
                            (material/with-reflective 0.5)))
                      (shape/with-transform
                        (transform/translation 0 -1 0)))
            w (world/add-object w shape)
            r (ray/ray (tup/point 0 0 -3)
                       (tup/vector 0 (- root-2-over-2) root-2-over-2))
            comps (i/prepare-computations (world/relations w) (i/intersection (Math/sqrt 2) shape) r)]

        (is (approx (colour/colour 0.87676 0.92434 0.82917)
                    (#'world/shade-hit w comps)))))))

(deftest reflections-with-parallel-reflective-surfaces
  (testing "mutually reflective surfaces"
    (let [mirror (shape/with-material
                   (plane/plane)
                   (material/with-reflective (material/material) 1))
          w (-> (default-world)
                (assoc :lights
                       [(light/point-light (tup/point 0 0 0)
                                           (colour/colour 1 1 1))])
                (world/add-object (shape/with-transform
                                    mirror
                                    (transform/translation 0 -1 0)))
                (world/add-object (shape/with-transform
                                    mirror
                                    (transform/translation 0 1 0))))]
      (world/colour-at w (ray/ray (tup/point 0 0 0)
                                  (tup/vector 0 1 0))))))

(deftest the-reflected-colour-at-the-maximum-recursion-depth
  (let [root-2-over-2 (/ (Math/sqrt 2) 2)
        s (-> (plane/plane)
              (shape/with-material
                (material/with-reflective (material/material) 0.5))
              (shape/with-transform
                (transform/translation 0 -1 0)))
        w (-> (default-world)
              (world/add-object s))
        r (ray/ray (tup/point 0 0 -3)
                   (tup/vector 0 (- root-2-over-2) root-2-over-2)
                   1)
        comps (i/prepare-computations
                (world/relations w)
                (i/intersection (Math/sqrt 2) s)
                r)]
    (is (= (colour/colour 0 0 0)
           (world/reflected-colour w comps)))))

(deftest the-refracted-colour-with-an-opaque-surface
  (let [w (default-world)
        shape (first (world/objects w))
        r (ray/ray (tup/point 0 0 -5)
                   (tup/vector 0 0 1))
        xs (i/intersections (i/intersection 4 shape)
                            (i/intersection 6 shape))
        comps (i/prepare-computations (world/relations w) (first xs) r xs)]
    (is (= (colour/colour 0 0 0)
           (world/refracted-colour w comps)))))

(deftest the-refracted-colour-at-the-maximum-recursion-depth
  (let [w (update-in (default-world)
                     [:objects 0 :material]
                     (fn [m]
                       (-> m
                           (material/with-transparency 1.0)
                           (material/with-refractive-index 1.5))))
        shape (first (world/objects w))
        r (ray/ray (tup/point 0 0 -5)
                   (tup/vector 0 0 1)
                   1)
        xs (i/intersections (i/intersection 4 shape)
                            (i/intersection 6 shape))
        comps (i/prepare-computations (world/relations w) (first xs) r xs)]
    (is (= (colour/colour 0 0 0)
           (world/refracted-colour w comps)))))

(deftest the-refracted-colour-under-total-internal-reflection
  (let [root-2-over-2 (/ (Math/sqrt 2) 2)
        w (update-in (default-world)
                     [:objects 0 :material]
                     (fn [m]
                       (-> m
                           (material/with-transparency 1.0)
                           (material/with-refractive-index 1.5))))
        shape (first (world/objects w))
        r (ray/ray (tup/point 0 0 root-2-over-2)
                   (tup/vector 0 1 0))
        xs (i/intersections (i/intersection (- root-2-over-2) shape)
                            (i/intersection root-2-over-2 shape))
        comps (i/prepare-computations (world/relations w) (second xs) r xs)]
    (is (= (colour/colour 0 0 0)
           (world/refracted-colour w comps)))))

(deftest the-refracted-colour-with-a-refracted-ray
  (let [w (-> (default-world)
              (update-in
                [:objects 0 :material]
                (fn [m]
                  (-> m
                      (material/with-ambient 1.0)
                      (material/with-pattern (test-pattern)))))
              (update-in
                [:objects 1 :material]
                (fn [m]
                  (-> m
                      (material/with-transparency 1.0)
                      (material/with-refractive-index 1.5)))))
        [a b] (world/objects w)
        r (ray/ray (tup/point 0 0 0.1)
                   (tup/vector 0 1 0))
        xs (i/intersections
             (i/intersection -0.9899 a)
             (i/intersection -0.4899 b)
             (i/intersection 0.4899 b)
             (i/intersection 0.9899 a))
        comps (i/prepare-computations (world/relations w) (nth xs 2) r xs)]
    (is (approx (colour/colour 0 0.99888 0.04721)
                (world/refracted-colour w comps)))))

(deftest shade-hit-with-a-transparent-material
  (let [root-2-over-2 (/ (Math/sqrt 2) 2)
        floor (-> (plane/plane)
                  (shape/with-transform (transform/translation 0 -1 0))
                  (shape/with-material (-> (material/material)
                                           (material/with-transparency 0.5)
                                           (material/with-refractive-index 1.5))))
        ball (-> (sphere/sphere)
                 (shape/with-transform (transform/translation 0 -3.5 -0.5))
                 (shape/with-material (-> (material/material)
                                          (material/with-colour (colour/colour 1 0 0))
                                          (material/with-ambient 0.5))))
        w (-> (default-world)
              (world/add-object floor)
              (world/add-object ball))
        r (ray/ray (tup/point 0 0 -3)
                   (tup/vector 0 (- root-2-over-2) root-2-over-2))
        xs (i/intersections (i/intersection (Math/sqrt 2) floor))
        comps (i/prepare-computations (world/relations w) (first xs) r xs)]
    (is  (approx (colour/colour 0.93642 0.68642 0.68642)
                 (#'world/shade-hit w comps)))))

(deftest shade-hit-with-a-reflective-transparent-material
  (let [root-2-over-2 (/ (Math/sqrt 2) 2)
        floor (-> (plane/plane)
                  (shape/with-transform (transform/translation 0 -1 0))
                  (shape/with-material (-> (material/material)
                                           (material/with-reflective 0.5)
                                           (material/with-transparency 0.5)
                                           (material/with-refractive-index 1.5))))
        ball (-> (sphere/sphere)
                 (shape/with-transform (transform/translation 0 -3.5 -0.5))
                 (shape/with-material (-> (material/material)
                                          (material/with-colour (colour/colour 1 0 0))
                                          (material/with-ambient 0.5))))
        w (-> (default-world)
              (world/add-object floor)
              (world/add-object ball))
        r (ray/ray (tup/point 0 0 -3)
                   (tup/vector 0 (- root-2-over-2) root-2-over-2))
        xs (i/intersections (i/intersection (Math/sqrt 2) floor))
        comps (i/prepare-computations (world/relations w) (first xs) r xs)]
    (is  (approx (colour/colour 0.93391 0.69643 0.69243)
                 (#'world/shade-hit w comps)))))
