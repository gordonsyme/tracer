(ns putting-together.chapter-6
  (:require [clojure.java.io :as io]
            [tracer.entities.canvas :as canvas]
            [tracer.entities.colour :as colour]
            [tracer.entities.intersection :as i]
            [tracer.entities.material :as material]
            [tracer.entities.light :as light]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as ray]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.transform :as t]
            [tracer.entities.tuple :as tup]
            [tracer.use-cases.ppm :as ppm]))

(defn go
  []
  (let [width 500
        height 500
        light (light/point-light (tup/point -10 10 10)
                                 (colour/colour 1 1 1))
        camera-pos (tup/point 0 0 5)
        sphere (-> (sphere/sphere)
                   (sphere/with-transform (-> (t/scaling 1 1 1)
                                              (t/shear :xy 1)
                                              (t/rotate-z (/ Math/PI 4))))
                   (sphere/with-material (assoc (material/material)
                                                :colour (colour/colour 1 0.2 1))))

        ;; wall is at z = -5 and has dimensions 6*6
        ;; transform world point to canvas ...
        ;; -3,  3 => 0 0
        ;; -3, -3 => 0 canvas.height
        ;;  3,  3 => canvas.width 0
        ;;  3, -3 => canvas.width canvas.height

        ; translate x by +3
        ; translate y by -3
        ; scale x by (/ (canvas/width) 6)
        ; scale y by (/ (canvas/height) 6)
        world-to-canvas (-> (t/identity)
                            (t/translate 3 -3 0)
                            (t/scale (/ width 6)
                                     (- (/ height 6)) ;; canvas y coordinates are zero at the top!
                                     1))
        canvas-to-world (mat/inverse world-to-canvas)
        pixels (map (fn [[x y]]
                      (let [target (t/apply canvas-to-world (tup/point x y -5))
                            r (ray/ray camera-pos
                                       (tup/normalise (tup/sub target camera-pos)))]
                        (when-let [hit (i/hit (i/intersect sphere r))]
                          (let [point (ray/position r (:t hit))
                                eye (tup/negate (:direction r))]
                            [x y (material/lighting (-> hit :object :material)
                                                    light
                                                    point
                                                    eye
                                                    (sphere/normal-at (:object hit) point))]))))
                    (for [x (range width)
                          y (range height)]
                      [x y]))
        canvas (reduce
                 (fn [c [x y colour]]
                   (canvas/write-pixel c x y colour))
                 (canvas/canvas width height)
                 (filter some? (seque pixels)))]
    (with-open [s (io/output-stream (io/as-file "/tmp/sphere-sheared-shaded.ppm"))]
      (ppm/generate canvas s))))
