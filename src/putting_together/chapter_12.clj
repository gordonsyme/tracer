(ns putting-together.chapter-12
  (:require [clojure.java.io :as io]
            [tracer.entities.world :as world]
            [tracer.entities.camera :as camera]
            [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.tuple :as tup]
            [tracer.entities.shape :as shape]
            [tracer.entities.cube :as cube]
            [tracer.entities.material :as material]
            [tracer.entities.transform :as transform]
            [tracer.use-cases.ppm :as ppm]))

(defn go
  [filename]
  (let [table-material (-> (material/material)
                           (material/with-colour (colour/colour 0.5 0.19 0)))
        table-rotation (/ Math/PI 3)
        table-top (-> (cube/cube)
                      (shape/with-transform
                        (-> (transform/identity)
                            (transform/scale 1.2 0.08 1.2)
                            (transform/translate 0 0.5 0)
                            (transform/rotate-y table-rotation)))
                      (shape/with-material table-material))
        table-leg-transform (-> (transform/identity)
                                (transform/scale 0.08 0.5 0.08))
        leg1 (-> (cube/cube)
                 (shape/with-transform
                   (-> table-leg-transform
                       (transform/translate 1 0 1)
                       (transform/rotate-y table-rotation)))
                 (shape/with-material table-material))
        leg2 (-> (cube/cube)
                 (shape/with-transform
                   (-> table-leg-transform
                       (transform/translate 1 0 -1)
                       (transform/rotate-y table-rotation)))
                 (shape/with-material table-material))
        leg3 (-> (cube/cube)
                 (shape/with-transform
                   (-> table-leg-transform
                       (transform/translate -1 0 1)
                       (transform/rotate-y table-rotation)))
                 (shape/with-material table-material))
        leg4 (-> (cube/cube)
                 (shape/with-transform
                   (-> table-leg-transform
                       (transform/translate -1 0 -1)
                       (transform/rotate-y table-rotation)))
                 (shape/with-material table-material))
        room (-> (cube/cube)
                 (shape/with-transform
                   (-> (transform/identity)
                       (transform/scale 6 2 6)
                       (transform/translate 0 1.5 0))))
        mirror (-> (cube/cube)
                   (shape/with-transform
                     (-> (transform/identity)
                         (transform/scale 3 1.5 0.08)
                         (transform/rotate-x (/ Math/PI -36))
                         (transform/translate 0 2 5.8)))
                   (shape/with-material
                     (-> (material/material)
                         (material/with-colour (colour/colour 0.1 0.1 0.1))
                         (material/with-diffuse 0.7)
                         (material/with-specular 0)
                         (material/with-reflective 0.8))))
        world (-> (world/world)
                  (world/add-light (light/point-light
                                     (tup/point -5 3.4 -5)
                                     (colour/colour 1 1 1)))
                  (world/add-object table-top)
                  (world/add-object leg1)
                  (world/add-object leg2)
                  (world/add-object leg3)
                  (world/add-object leg4)
                  (world/add-object room)
                  (world/add-object mirror))
        camera (camera/with-transform
                 (camera/camera 1440 720 (/ Math/PI 3))
                 (transform/view-transform (tup/point 0 1.5 -5)
                                           (tup/point 0 1 0)
                                           (tup/vector 0 1 0)))]
    (with-open [s (io/output-stream (io/as-file filename))]
      (ppm/generate (camera/render camera world) s))))
