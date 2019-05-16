(ns tracer.test-entities
  (:require [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.material :as material]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.entities.world :as world]))

(defn default-world
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
