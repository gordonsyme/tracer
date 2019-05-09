(ns putting-together.chapter-5
  (:require [clojure.java.io :as io]
            [tracer.entities.canvas :as canvas]
            [tracer.entities.colour :as colour]
            [tracer.entities.intersection :as i]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as ray]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.transform :as t]
            [tracer.entities.tuple :as tup]
            [tracer.use-cases.ppm :as ppm]))

(defn go
  []
  (let [width 200
        height 200
        light (tup/point 0 0 5)
        sphere (sphere/with-transform (sphere/sphere)
                                      (-> (t/scaling 1 1 1)
                                          (t/shear :xy 1)
                                          (t/rotate-z (/ Math/PI 4))))

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
        canvas (reduce
                 (fn [c [x y]]
                   (let [target (t/apply canvas-to-world (tup/point x y -5))
                         r (ray/ray light (tup/normalise (tup/sub target light)))]
                     (if (i/hit (i/intersect sphere r))
                       (canvas/write-pixel c x y (colour/colour 1.0 0 0))
                       c)))
                 (canvas/canvas width height)
                 (for [x (range width)
                       y (range height)]
                   [x y]))]
    (with-open [s (io/output-stream (io/as-file "/tmp/sphere-shadow.ppm"))]
      (ppm/generate canvas s))))
