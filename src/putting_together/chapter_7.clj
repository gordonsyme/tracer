(ns putting-together.chapter-7
  (:require [clojure.java.io :as io]
            [tracer.entities.world :as world]
            [tracer.entities.camera :as camera]
            [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.tuple :as tup]
            [tracer.entities.shape :as shape]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.material :as material]
            [tracer.entities.transform :as transform]
            [tracer.use-cases.ppm :as ppm]))

(defn go
  [filename]
  (let [surface-material (-> (material/material)
                             (material/with-colour (colour/colour 1 0.9 0.9))
                             (material/with-specular 0))
        floor (-> (sphere/sphere)
                  (shape/with-transform
                    (transform/scaling 10 0.01 10))
                  (shape/with-material surface-material))
        left-wall (-> (sphere/sphere)
                      (shape/with-transform
                        (-> (transform/identity)
                            (transform/scale 10 0.01 10)
                            (transform/rotate-x (/ Math/PI 2))
                            (transform/rotate-y (- (/ Math/PI 4)))
                            (transform/translate 0 0 5)))
                      (shape/with-material surface-material))
        right-wall (-> (sphere/sphere)
                       (shape/with-transform
                         (-> (transform/identity)
                             (transform/scale 10 0.01 10)
                             (transform/rotate-x (/ Math/PI 2))
                             (transform/rotate-y (/ Math/PI 4))
                             (transform/translate 0 0 5)))
                       (shape/with-material surface-material))
        middle-sphere (-> (sphere/sphere)
                          (shape/with-transform (transform/translation -0.5 1 0.5))
                          (shape/with-material
                            (-> (material/material)
                                (material/with-colour (colour/colour 0.1 1 0.5))
                                (material/with-diffuse 0.7)
                                (material/with-specular 0.3))))
        right-sphere (-> (sphere/sphere)
                         (shape/with-transform
                           (-> (transform/scaling 0.5 0.5 0.5)
                               (transform/translate 1.5 0.5 -0.5)))
                         (shape/with-material
                           (-> (material/material)
                               (material/with-colour (colour/colour 0.5 1 0.1))
                               (material/with-diffuse 0.7)
                               (material/with-specular 0.3))))
        left-sphere (-> (sphere/sphere)
                        (shape/with-transform
                          (-> (transform/scaling 0.33 0.33 0.33)
                              (transform/translate -1.5 0.33 -0.75)))
                        (shape/with-material
                          (-> (material/material)
                              (material/with-colour (colour/colour 1 0.8 0.1))
                              (material/with-diffuse 0.7)
                              (material/with-specular 0.3))))
        world (-> (world/world)
                  (world/add-light (light/point-light
                                     (tup/point -10 10 -10)
                                     (colour/colour 1 1 1)))
                  (world/add-object floor)
                  (world/add-object left-wall)
                  (world/add-object right-wall)
                  (world/add-object middle-sphere)
                  (world/add-object right-sphere)
                  (world/add-object left-sphere))
        camera (camera/with-transform
                 (camera/camera 500 250 (/ Math/PI 3))
                 (transform/view-transform (tup/point 0 1.5 -5)
                                           (tup/point 0 1 0)
                                           (tup/vector 0 1 0)))]
    (with-open [s (io/output-stream (io/as-file filename))]
      (ppm/generate (camera/render camera world) s))))

(defn go-lights
  [filename]
  (let [sphere (-> (sphere/sphere)
                   (shape/with-transform (transform/scaling 2 2 2))
                   (shape/with-material
                     (-> (material/material)
                         (material/with-colour (colour/colour 1 1 1)))))
        world (-> (world/world)
                  (world/add-light (light/point-light
                                     (tup/point -10 10 -10)
                                     (colour/colour 1 0 0)))
                  (world/add-light (light/point-light
                                     (tup/point 10 10 -10)
                                     (colour/colour 0 1 0)))
                  (world/add-light (light/point-light
                                     (tup/point 0 -10 -10)
                                     (colour/colour 0 0 1)))
                  (world/add-object sphere))
        camera (camera/with-transform
                 (camera/camera 500 500 (/ Math/PI 3))
                 (transform/view-transform (tup/point 0 0 -5)
                                           (tup/point 0 0 0)
                                           (tup/vector 0 1 0)))]
    (with-open [s (io/output-stream (io/as-file filename))]
      (ppm/generate (camera/render camera world) s))))
