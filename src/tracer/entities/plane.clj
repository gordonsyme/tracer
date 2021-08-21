(ns tracer.entities.plane
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.material :as material]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.math :refer (EPSILON)]))

(s/def ::plane ::shape/common)

(defmethod shape/object-type :plane
  [_]
  ::plane)

(defn plane
  []
  {::shape/tag :plane
   ::shape/id (java.util.UUID/randomUUID)
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)})
(s/fdef plane
  :ret ::plane)

(defn- local-intersect
  [ray]
  (if (< (Math/abs ^double (tup/y (ray/direction ray)))
         EPSILON)
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

(defmethod shape/bounds :plane
  [_rels _p]
  ;; A plane is defined as all points where y = 0. In this case we want to
  ;; force intersection checking when the ray comes close to the plane due to
  ;; imprecision of floating point operations, so we use -/+ EPSILON as the
  ;; min/max y-coordinates of the bounding box.
  {:min-bound (tup/point (- Double/NEGATIVE_INFINITY) (- EPSILON) (- Double/NEGATIVE_INFINITY))
   :max-bound (tup/point Double/POSITIVE_INFINITY EPSILON Double/POSITIVE_INFINITY)})
