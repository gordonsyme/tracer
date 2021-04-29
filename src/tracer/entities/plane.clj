(ns tracer.entities.plane
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.material :as material]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            ))

(s/def ::plane ::shape/common)

(defmethod shape/object-type :plane
  [_]
  ::plane)

(defn plane
  []
  {::shape/tag :plane
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)})
(s/fdef plane
  :ret ::plane)

(defn- local-intersect
  [ray]
  (if (< (Math/abs ^double (tup/y (ray/direction ray)))
         0.00001)
    []
    [(/ (- (tup/y (ray/origin ray)))
        (tup/y (ray/direction ray)))]))
(s/fdef local-intersect
  :args (s/cat :ray ::ray/ray)
  :ret (s/coll-of ::shape/t))

(defmethod shape/local-intersect :plane
  [_rels plane ray]
  (map (partial hash-map :object plane :t)
       (local-intersect ray)))

(defmethod shape/local-normal-at :plane
  [_s _p]
  (tup/vector 0 1 0))
