(ns putting-together.chapter-2
  (:require [clojure.spec.alpha :as s]
            [clojure.java.io :as io]
            [tracer.entities.canvas :as canvas]
            [tracer.entities.colour :as colour]
            [tracer.entities.tuple :as t]
            [tracer.use-cases.ppm :as ppm]
            [putting-together.chapter-1 :as ch1]))

(defn write-pixels
  [c x y colour]
  (-> c
      (canvas/write-pixel x y colour)
      (canvas/write-pixel x (inc y) colour)
      (canvas/write-pixel (inc x) y colour)
      (canvas/write-pixel (inc x) (inc y) colour)))
(s/fdef write-pixels
  :args (s/cat :c ::canvas/canvas
               :x nat-int?
               :y nat-int?
               :colour ::colour/colour)
  :ret ::canvas/canvas)

(defn go
  []
  (let [start (t/point 0 1 0)
        velocity (-> (t/vector 1 1.8 0)
                     (t/normalise)
                     (t/mul 11.25))
        projectile (ch1/projectile start velocity)
        gravity (t/vector 0 -0.1 0)
        wind (t/vector -0.01 0 0)
        env (ch1/environment gravity wind)
        reddish (colour/colour 0.9 0.2 0)
        get-pos #(-> % second :position)
        canvas (reduce
                 (fn [c p]
                   (write-pixels c (int (t/x p)) (- 550 (int (t/y p))) reddish))
                 (canvas/canvas 900 550)
                 (->> (iterate ch1/tick [env projectile])
                      (take-while #(-> % get-pos second (>= 0)))
                      (map get-pos)))]
    (with-open [s (io/output-stream (io/as-file "/tmp/picture.ppm"))]
      (ppm/generate canvas s))))
