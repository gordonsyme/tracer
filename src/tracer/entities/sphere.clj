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
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)})
(s/fdef sphere
  :ret ::sphere)

(defmethod shape/local-intersect :sphere
  [_s ray]
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

(defmethod shape/local-normal-at :sphere
  [_s p]
  (tup/sub p (tup/point 0 0 0)))
