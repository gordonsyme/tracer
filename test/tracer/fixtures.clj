(ns tracer.fixtures
  (:require [orchestra.spec.test :as s-test]))

(defn instrument
  [t]
  (try
    (s-test/instrument)
    (t)
    (finally
      (s-test/unstrument))))
