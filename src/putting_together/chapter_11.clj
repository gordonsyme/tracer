(ns putting-together.chapter-11
  (:require [clojure.java.io :as io]
            [tracer.entities.world :as world]
            [tracer.entities.camera :as camera]
            [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.pattern :as pattern]
            [tracer.entities.plane :as plane]
            [tracer.entities.tuple :as tup]
            [tracer.entities.shape :as shape]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.material :as material]
            [tracer.entities.transform :as transform]
            [tracer.use-cases.ppm :as ppm]))

(defn go-schlick-test
  [filename]
  (let [checked (pattern/with-transform
                  (pattern/checks (colour/colour 0 0 0)
                                  (colour/colour 1 1 1))
                  (-> (transform/identity)
                      (transform/scale 1.5 1.5 1.5)
                      (transform/translate 0 0.1 0)))
        floor (-> (plane/plane)
                  (shape/with-transform (transform/translation 0 -10.1 0))
                  (shape/with-material
                    (material/with-pattern (material/material) checked)))
        glass-sphere (-> (sphere/sphere)
                         (shape/with-material
                           (-> (material/glass)
                               (material/with-diffuse 0.1)
                               (material/with-shininess 300))))
        air-sphere (-> (sphere/sphere)
                       (shape/with-transform (transform/scaling 0.5 0.5 0.5))
                       (shape/with-material (material/air)))
        world (-> (world/world)
                  (world/add-light (light/point-light
                                     (tup/point 10 10 0)
                                     (colour/colour 0.7 0.7 0.7)))
                  (world/add-object floor)
                  (world/add-object glass-sphere)
                  (world/add-object air-sphere))
        camera (camera/with-transform
                 (camera/camera 250 250 (/ Math/PI 3))
                 (transform/view-transform (tup/point 0 2.5 0)
                                           (tup/point 0 0 0)
                                           (tup/vector 0 0 1)))]
    (with-open [s (io/output-stream (io/as-file filename))]
      (ppm/generate (camera/render camera world) s))))

(defn go
  [filename]
  (let [blue1 (colour/colour 0.03 0.23 0.81)
        blue2 (colour/colour 0.25 0.44 1)
        red1 (colour/colour 0.66 0.02 0.36)
        red2 (colour/colour 0.9 0.21 0.56)
        yellow (colour/colour 0.9 0.78 0.18)
        stripes (pattern/with-transform
                  (pattern/stripes blue1 blue2)
                  (transform/scaling 0.2 0.2 0.2))
        target (pattern/with-transform
                 (pattern/rings red1 (colour/colour 1.0 1.0 1.0))
                 (-> (transform/identity)
                     (transform/scale 0.5 0.5 0.5)
                     (transform/translate 3 0 -1.5)))
        floor-pattern (pattern/checked-pattern (pattern/with-transform
                                                 (pattern/stripe-pattern (pattern/colour-pattern blue1)
                                                                         (pattern/colour-pattern blue2))
                                                 (-> (transform/identity)
                                                     (transform/scale 0.2 0.2 0.2)
                                                     (transform/rotate-y (/ Math/PI 3))))
                                               (pattern/with-transform
                                                 (pattern/stripe-pattern (pattern/colour-pattern red1)
                                                                         (pattern/colour-pattern red2))
                                                 (-> (transform/identity)
                                                     (transform/scale 0.2 0.2 0.2)
                                                     (transform/rotate-y (/ Math/PI -3)))))
        yellow-gingham (let [t (transform/scaling 0.4 0.4 0.4)]
                         (pattern/blended-pattern
                           (pattern/with-transform
                             (pattern/stripes yellow (colour/colour 1.0 1.0 1.0)) t)
                           (pattern/with-transform
                             (pattern/stripes yellow (colour/colour 1.0 1.0 1.0))
                             (transform/rotate-y t (/ Math/PI 2)))))
        surface-material (-> (material/material)
                             (material/with-colour (colour/colour 1 0.9 0.9))
                             (material/with-specular 0))
        floor (shape/with-material
                (plane/plane)
                (-> (material/material)
                    (material/with-pattern floor-pattern)
                    (material/with-reflective 0.25)))
        left-wall (-> (plane/plane)
                      (shape/with-transform
                        (-> (transform/identity)
                            (transform/rotate-x (/ Math/PI 2))
                            (transform/rotate-y (- (/ Math/PI 4)))
                            (transform/translate 0 0 5)))
                      (shape/with-material
                        (material/with-pattern surface-material
                                               yellow-gingham)))
        right-wall (-> (plane/plane)
                       (shape/with-transform
                         (-> (transform/identity)
                             (transform/rotate-x (/ Math/PI 2))
                             (transform/rotate-y (/ Math/PI 4))
                             (transform/translate 0 0 5)))
                       (shape/with-material (material/with-pattern surface-material target)))
        middle-sphere (-> (sphere/sphere)
                          (shape/with-transform (transform/translation -0.5 1 0.5))
                          (shape/with-material
                            (-> (material/material)
                                #_(material/with-colour (colour/colour 0.1 1 0.5))
                                (material/with-colour (colour/colour 0.1 0.1 0.1))
                                (material/with-diffuse 0.7)
                                (material/with-specular 0)
                                #_(material/with-pattern checkers)
                                (material/with-reflective 0.7))))
        right-sphere (-> (sphere/sphere)
                         (shape/with-transform
                           (-> (transform/scaling 0.5 0.5 0.5)
                               (transform/translate 1.5 0.5 -0.5)))
                         (shape/with-material
                           (-> (material/glass)
                               (material/with-colour (colour/colour 0.1 0.1 0.1))
                               (material/with-diffuse 0.1)
                               (material/with-transparency 0.9)
                               (material/with-specular 0.3))))
        left-sphere (-> (sphere/sphere)
                        (shape/with-transform
                          (-> (transform/scaling 0.33 0.33 0.33)
                              (transform/translate -1.5 0.33 -0.75)))
                        (shape/with-material
                          (-> (material/material)
                              (material/with-colour (colour/colour 1 0.8 0.1))
                              (material/with-diffuse 0.7)
                              (material/with-specular 0.3)
                              (material/with-pattern
                                (pattern/with-transform stripes
                                  (-> (transform/scaling 0.1 0.1 0.1)
                                      (transform/rotate-z (/ Math/PI 3))))))))
        world (-> (world/world)
                  (world/add-light (light/point-light
                                     (tup/point -10 10 -10)
                                     (colour/colour 1 1 1)))
                  #_(world/add-light (light/point-light
                                     (tup/point 10 10 -10)
                                     (colour/colour 0 0 1)))
                  (world/add-object floor)
                  (world/add-object left-wall)
                  (world/add-object right-wall)
                  (world/add-object middle-sphere)
                  (world/add-object right-sphere)
                  (world/add-object left-sphere))
        camera (camera/with-transform
                 (camera/camera 1440 720 (/ Math/PI 3))
                 (transform/view-transform (tup/point 0 1.5 -5)
                                           (tup/point 0 1 0)
                                           (tup/vector 0 1 0)))]
    (with-open [s (io/output-stream (io/as-file filename))]
      (ppm/generate (camera/render camera world) s))))
