(ns tracer.entities.sphere
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.intersection :as i]
            [tracer.entities.material :as material]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as ray]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as t]))

(s/def ::sphere ::i/object-common)

(defmethod i/object-type :sphere
  [_]
  ::sphere)

(defn sphere
  []
  {::i/tag :sphere
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)})
(s/fdef sphere
  :ret ::sphere)

(defmethod i/intersect :sphere
  [s ray]
  (let [r (ray/transform ray (:inverse-transform s))
        sphere-to-ray (t/sub (:origin r) (t/point 0 0 0))
        a (t/dot (:direction r) (:direction r))
        b (* 2 (t/dot (:direction r)
                      sphere-to-ray))
        c (dec (t/dot sphere-to-ray sphere-to-ray))
        discriminant (- (* b b)
                        (* 4 a c))]
    (if (neg? discriminant)
      []
      (let [root (Math/sqrt discriminant)
            ts [(/ (- (- b) root)
                   (* 2 a))
                (/ (+ (- b) root)
                   (* 2 a))]]
        (map (partial hash-map :object s :t) ts)))))

(defn with-transform
  [s m]
  (assoc s :transform m
           :inverse-transform (mat/inverse m)))
(s/fdef with-transform
  :args (s/cat :s ::sphere
               :m ::mat/matrix)
  :ret ::sphere)

(defn with-material
  [s m]
  (assoc s :material m))
(s/fdef with-material
  :args (s/cat :s ::sphere
               :m ::material/material)
  :ret ::sphere)

(defmethod i/normal-at :sphere
  [s p]
  (let [inv-transform (:inverse-transform s)
        object-point (mat/mult inv-transform p)
        object-normal (t/sub object-point (t/point 0 0 0))]
    (t/normalise
      ;; Setting w=0.0 is a hack to account for transposes of translations
      ;; mucking with the w coordinate of vectors.
      ;; The correct thing to do is multiply by the inverse of (submatrix
      ;; transform 3 3) but that reduces 4-component tuples down to 3-component
      ;; tuples.
      (assoc (mat/mult (mat/transpose inv-transform) object-normal)
             3
             0.0))))
