(ns tracer.entities.material-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.colour :as colour]
            [tracer.entities.material :as material]
            [tracer.entities.light :as light]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest the-default-material
  (let [m (material/material)]
    (is (= {:colour (colour/colour 1 1 1)
            :ambient 0.1
            :diffuse 0.9
            :specular 0.9
            :shininess 200.0}
           m))))

(deftest lighting-tests
  (let [root-2-over-2 (/ (Math/sqrt 2) 2)
        m (material/material)
        point (tup/point 0 0 0)]
    (testing "With the eye between the light and the surface"
      (let [eyev (tup/vector 0 0 -1)
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 0 -10)
                                     (colour/colour 1 1 1))]
        (is (= (colour/colour 1.9 1.9 1.9)
               (material/lighting m light point eyev normalv)))))

    (testing "With the eye between the light and the surface, eye offset 45 degrees"
      (let [eyev (tup/vector 0 root-2-over-2 (- root-2-over-2))
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 0 -10)
                                     (colour/colour 1 1 1))]
        (is (= (colour/colour 1.0 1.0 1.0)
               (material/lighting m light point eyev normalv)))))

    (testing "With the eye opposite the surface, light offset 45 degrees"
      (let [eyev (tup/vector 0 0 -1)
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 10 -10)
                                     (colour/colour 1 1 1))]
        (is (approx (colour/colour 0.736396 0.736396 0.736396)
                    (material/lighting m light point eyev normalv)))))

    (testing "With the eye opposite the surface, light offset 45 degrees"
      (let [eyev (tup/vector 0 (- root-2-over-2) (- root-2-over-2))
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 10 -10)
                                     (colour/colour 1 1 1))]
        (is (approx (colour/colour 1.636396 1.636396 1.636396)
                    (material/lighting m light point eyev normalv)))))

    (testing "With the light behind the surface"
      (let [eyev (tup/vector 0 0 -1)
            normalv (tup/vector 0 0 -1)
            light (light/point-light (tup/point 0 0 10)
                                     (colour/colour 1 1 1))]
        (is (= (colour/colour 0.1 0.1 0.1)
               (material/lighting m light point eyev normalv)))))))
