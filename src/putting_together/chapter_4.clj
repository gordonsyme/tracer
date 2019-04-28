(ns putting-together.chapter-4
  (:require [clojure.java.io :as io]
            [tracer.entities.canvas :as canvas]
            [tracer.entities.colour :as colour]
            [tracer.entities.transform :as t]
            [tracer.entities.tuple :as tup]
            [tracer.use-cases.ppm :as ppm]
            [putting-together.chapter-2 :refer (write-pixels)]))

(defn go
  []
  (let [tforms (take 12
                     (iterate #(t/rotate-y % (/ Math/PI 6))
                              (t/translation 0 0 100)))
        points (map #(t/apply % (tup/point 0 0 0))
                    tforms)
        canvas (reduce (fn [c p]
                         (let [p (t/apply (t/translation
                                            (/ (canvas/width c) 2)
                                            0
                                            (/ (canvas/height c) 2))
                                          p)
                               x (int (tup/x p))
                               z (int (tup/z p))]
                           (write-pixels c x z (colour/colour 1.0 1.0 1.0))))
                       (canvas/canvas 250 250)
                       points)]
    (with-open [s (io/output-stream (io/as-file "/tmp/clock.ppm"))]
      (ppm/generate canvas s))))
