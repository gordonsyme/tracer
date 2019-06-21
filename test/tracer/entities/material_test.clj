(ns tracer.entities.material-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.colour :as colour]
            [tracer.entities.material :as material]
            [tracer.entities.light :as light]
            [tracer.entities.pattern :as pattern]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest the-default-material
  (let [m (material/material)]
    (is (= {:colour (colour/colour 1 1 1)
            :ambient 0.1
            :diffuse 0.9
            :specular 0.9
            :shininess 200.0
            :reflective 0.0
            :pattern nil}
           m))))

(deftest lighting-tests
  (let [root-2-over-2 (/ (Math/sqrt 2) 2)
        m (material/material)
        point (tup/point 0 0 0)
        shader (fn [_point]
                 (:colour m))]
    (testing "With the eye between the light and the surface"
      (let [eyev (tup/vector 0 0 -1)
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 0 -10)
                                     (colour/colour 1 1 1))]
        (is (= (colour/colour 1.9 1.9 1.9)
               (material/lighting m shader light point eyev normalv false)))))

    (testing "With the eye between the light and the surface, eye offset 45 degrees"
      (let [eyev (tup/vector 0 root-2-over-2 (- root-2-over-2))
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 0 -10)
                                     (colour/colour 1 1 1))]
        (is (= (colour/colour 1.0 1.0 1.0)
               (material/lighting m shader light point eyev normalv false)))))

    (testing "With the eye opposite the surface, light offset 45 degrees"
      (let [eyev (tup/vector 0 0 -1)
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 10 -10)
                                     (colour/colour 1 1 1))]
        (is (approx (colour/colour 0.736396 0.736396 0.736396)
                    (material/lighting m shader light point eyev normalv false)))))

    (testing "With the eye opposite the surface, light offset 45 degrees"
      (let [eyev (tup/vector 0 (- root-2-over-2) (- root-2-over-2))
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 10 -10)
                                     (colour/colour 1 1 1))]
        (is (approx (colour/colour 1.636396 1.636396 1.636396)
                    (material/lighting m shader light point eyev normalv false)))))

    (testing "With the light behind the surface"
      (let [eyev (tup/vector 0 0 -1)
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 0 10)
                                     (colour/colour 1 1 1))]
        (is (= (colour/colour 0.1 0.1 0.1)
               (material/lighting m shader light point eyev normalv false)))))

    (testing "With the surface in shadow"
      (let [eyev (tup/vector 0 0 -1)
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 0 -10)
                                     (colour/colour 1 1 1))
            in-shadow true]
        (is (= (colour/colour 0.1 0.1 0.1)
               (material/lighting m shader light point eyev normalv in-shadow)))))))

(deftest lighting-with-patterns
  (let [m (-> (material/material)
              (material/with-ambient 1)
              (material/with-diffuse 0)
              (material/with-specular 0)
              (material/with-pattern
                (pattern/stripes (colour/colour 1 1 1)
                                 (colour/colour 0 0 0))))
        eyev (tup/vector 0 0 -1)
        normalv (tup/vector 0 0 -1)
        light (light/point-light (tup/point 0 0 -10)
                                 (colour/colour 1 1 1))
        shader (partial pattern/colour-at
                        (:pattern m)
                        (transform/identity))]
    (is (= (colour/colour 1 1 1)
           (material/lighting m shader light (tup/point 0.9 0 0) eyev normalv false)))
    (is (= (colour/colour 0 0 0)
           (material/lighting m shader light (tup/point 1.1 0 0) eyev normalv false)))))
