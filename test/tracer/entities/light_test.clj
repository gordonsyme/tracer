(ns tracer.entities.light-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest a-point-light-has-position-and-intensity
  (let [intensity (colour/colour 1 1 1)
        position (tup/point 0 0 0)
        light (light/point-light position intensity)]
    (is (= {:position position
            :intensity intensity}
           light))))
