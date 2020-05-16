(ns tracer.math
  (:refer-clojure :exclude [zero?]))

(def EPSILON 0.00001)

(defn zero?
  [^double a]
  (< (Math/abs a) EPSILON))
