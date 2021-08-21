(ns tracer.comparators)

(defn approx
  "Compare two tuples approximately"
  [v1 v2]
  (cond
    (and (sequential? v1)
         (sequential? v2))
    (every? true? (map approx v1 v2))

    :else
    (< (Math/abs (- ^double v1 ^double v2))
       0.00001)))
