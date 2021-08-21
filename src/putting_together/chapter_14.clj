(ns putting-together.chapter-14
  (:require [clojure.java.io :as io]
            [tracer.entities.camera :as camera]
            [tracer.entities.colour :as colour]
            [tracer.entities.cone :as cone]
            [tracer.entities.cube :as cube]
            [tracer.entities.cylinder :as cylinder]
            [tracer.entities.group :as group]
            [tracer.entities.light :as light]
            [tracer.entities.material :as material]
            [tracer.entities.pattern :as pattern]
            [tracer.entities.plane :as plane]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.entities.world :as world]
            [tracer.use-cases.ppm :as ppm]))

(defn- table
  [rels]
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
        table-group (group/group)
        new-rels (group/add-children rels table-group [table-top leg1 leg2 leg3 leg4])]
    [new-rels table-group]))

(defn- towers-of-hanoi
  [rels]
  (let [cyl1 (-> (cylinder/cylinder)
                 (cylinder/with-minimum 0)
                 (cylinder/with-maximum 0.4)
                 (cylinder/with-closed true)
                 (shape/with-material
                   (-> (material/material)
                       (material/with-reflective 0.2)
                       (material/with-colour (colour/colour 0.8 0 0))))
                 (shape/with-transform
                   (-> (transform/identity)
                       (transform/scale 0.08 1 0.08))))
        cyl2 (-> (cylinder/cylinder)
                 (cylinder/with-minimum 0)
                 (cylinder/with-maximum 0.32)
                 (cylinder/with-closed true)
                 (shape/with-material
                   (-> (material/material)
                       (material/with-reflective 0.2)
                       (material/with-colour (colour/colour 0 0.8 0))))
                 (shape/with-transform
                   (-> (transform/identity)
                       (transform/scale 0.16 1 0.16))))
        cyl3 (-> (cylinder/cylinder)
                 (cylinder/with-minimum 0)
                 (cylinder/with-maximum 0.24)
                 (cylinder/with-closed true)
                 (shape/with-material
                   (-> (material/material)
                       (material/with-reflective 0.2)
                       (material/with-colour (colour/colour 0 0 0.8))))
                 (shape/with-transform
                   (-> (transform/identity)
                       (transform/scale 0.24 1 0.24))))
        cyl4 (-> (cylinder/cylinder)
                 (cylinder/with-minimum 0)
                 (cylinder/with-maximum 0.16)
                 (cylinder/with-closed true)
                 (shape/with-material
                   (-> (material/material)
                       (material/with-reflective 0.2)
                       (material/with-colour (colour/colour 1 0.8 0))))
                 (shape/with-transform
                   (-> (transform/identity)
                       (transform/scale 0.32 1 0.32))))
        cyl5 (-> (cylinder/cylinder)
                 (cylinder/with-minimum 0)
                 (cylinder/with-maximum 0.08)
                 (cylinder/with-closed true)
                 (shape/with-material
                   (-> (material/material)
                       (material/with-reflective 0.2)
                       (material/with-colour (colour/colour 0.8 0 1))))
                 (shape/with-transform
                   (-> (transform/identity)
                       (transform/scale 0.4 1 0.4))))
        hanoi-group (shape/with-transform
                      (group/group)
                      (-> (transform/identity)
                          (transform/translate 0.5 0.58 -0.5)))
        new-rels (group/add-children rels hanoi-group [cyl1 cyl2 cyl3 cyl4 cyl5])]
    [new-rels hanoi-group]))

(defn- cone
  []
  (-> (cone/cone)
      (cone/with-minimum -0.25)
      (cone/with-maximum 0.25)
      (cone/with-closed true)
      (shape/with-transform
        (-> (transform/identity)
            (transform/translate 0.2 0.8 0)))))

(defn go
  [filename]
  (let [checked (pattern/with-transform
                  (pattern/checks (colour/colour 0 0 0)
                                  (colour/colour 1 1 1))
                  (-> (transform/identity)
                      (transform/rotate-y (/ Math/PI 7))
                      (transform/scale 1.5 1.5 1.5)
                      (transform/translate 0 0.1 0)))
        floor (-> (plane/plane)
                  (shape/with-material
                    (-> (material/material)
                        (material/with-pattern checked)
                        (material/with-reflective 0.2)))
                  (shape/with-transform
                    (transform/translation 0 -0.495 0)))
        room (-> (cube/cube)
                 (shape/with-transform
                   (-> (transform/identity)
                       (transform/scale 6 2 6)
                       (transform/translate 0 1.5 0))))
        mirror (-> (cube/cube)
                   (shape/with-transform
                     (-> (transform/identity)
                         (transform/scale 3 1.5 0.02)
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
                                     (tup/point 2 3.4 -2)
                                     (colour/colour 1 1 1)))
                  (world/add-object room)
                  (world/add-object mirror)
                  (world/add-object floor))
        rels (world/relations world)
        [rels table-group] (table rels)
        [rels hanoi-group] (towers-of-hanoi rels)

        stuff (group/group)
        rels (group/add-children rels stuff [table-group hanoi-group (cone)])

        world (world/add-relations world rels)
        world (world/add-object world stuff)

        camera (camera/with-transform
                 (camera/camera 1440 800 (/ Math/PI 3))
                 (transform/view-transform (tup/point 0 2 -5)
                                           (tup/point 0 1 0)
                                           (tup/vector 0 1 0)))]
    (with-open [s (io/output-stream (io/as-file filename))]
      (ppm/generate (camera/render camera world) s))))
