(ns tracer.entities.sphere
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.material :as material]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(s/def ::sphere ::shape/common)

(defmethod shape/object-type :sphere
  [_]
  ::sphere)

(defn sphere
  []
  {::shape/tag :sphere
   ::shape/id (java.util.UUID/randomUUID)
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)})
(s/fdef sphere
  :ret ::sphere)

(defn- local-intersect
  [ray]
  (let [sphere-to-ray (tup/sub (ray/origin ray) (tup/point 0 0 0))
        a (tup/dot (ray/direction ray) (ray/direction ray))
        b (* 2 (tup/dot (ray/direction ray)
                      sphere-to-ray))
        c (dec (tup/dot sphere-to-ray sphere-to-ray))
        discriminant (- (* b b)
                        (* 4 a c))]
    (if (neg? discriminant)
      []
      (let [root (Math/sqrt discriminant)]
        [(/ (- (- b) root)
            (* 2 a))
         (/ (+ (- b) root)
            (* 2 a))]))))
(s/fdef local-intersect
  :args (s/cat :ray ::ray/ray)
  :ret (s/coll-of ::shape/t))

(defmethod shape/local-intersect :sphere
  [_rels s ray]
  (map (partial hash-map :object s :t)
       (local-intersect ray)))

(defmethod shape/local-normal-at :sphere
  [_s p]
  (tup/sub p (tup/point 0 0 0)))
