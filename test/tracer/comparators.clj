(ns tracer.comparators
  (:require [tracer.types :as types]))

(defn approx
  "Compare two tuples approximately"
  [v1 v2]
  (cond
    (or (types/double-array? v1)
        (types/double-array? v2))
    (approx (into [] v1) (into [] v2))

    (and (sequential? v1)
         (sequential? v2))
    (every? true? (map approx v1 v2))

    :else
    (< (Math/abs (- v1 v2))
       0.00001)))

(defn- base-eq
  "Compare two tuples for equality"
  [v1 v2]
  (cond
    (or (types/double-array? v1)
        (types/double-array? v2))
    (= (into [] v1) (into [] v2))

    (and (sequential? v1)
         (sequential? v2))
    (every? true? (map = v1 v2))

    :else
    (zero? (- v1 v2))))

(defn eq
  [v & vs]
  (reduce (fn [acc v2]
            (and acc
                 (base-eq v v2)))
          true
          vs))
