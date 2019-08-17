(ns tracer.entities.cylinder
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.material :as material]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(s/def ::minimum double?)
(s/def ::maximum double?)

(s/def ::cylinder (s/merge ::shape/common
                           (s/keys :req-un [::minimum ::maximum])))

(defmethod shape/object-type :cylinder
  [_]
  ::cylinder)

(defn cylinder
  []
  {::shape/tag :cylinder
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)
   :minimum Double/NEGATIVE_INFINITY
   :maximum Double/POSITIVE_INFINITY})
(s/fdef cylinder
  :ret ::cylinder)

(defn with-minimum
  [c minimum]
  (assoc c :minimum minimum))
(s/fdef with-minimum
  :args (s/cat :c ::cylinder
               :minimum double?)
  :ret ::cylinder)

(defn with-maximum
  [c maximum]
  (assoc c :maximum maximum))
(s/fdef with-maximum
  :args (s/cat :c ::cylinder
               :maximum double?)
  :ret ::cylinder)

(defmethod shape/local-intersect :cylinder
  [cyl ray]
  (let [origin (ray/origin ray)
        direction (ray/direction ray)
        dir-x (tup/x direction)
        dir-z (tup/z direction)
        a (+ (* dir-x dir-x)
             (* dir-z dir-z))]
    (if (< (Math/abs ^double a) 0.00001)
      []
      (let [origin-x (tup/x origin)
            origin-z (tup/z origin)
            b (+ (* 2 origin-x dir-x)
                 (* 2 origin-z dir-z))
            c (dec (+ (* origin-x origin-x)
                      (* origin-z origin-z)))
            discriminant (- (* b b)
                            (* 4 a c))
            ts (if (neg? discriminant)
                 []
                 (let [root (Math/sqrt discriminant)]
                   [(/ (- (- b) root)
                       (* 2 a))
                    (/ (+ (- b) root)
                       (* 2 a))]))]
        (filter (fn [t]
                  (< (:minimum cyl)
                     (+ (tup/y origin)
                        (* t (tup/y direction)))
                     (:maximum cyl)))
                (sort ts))))))

(defmethod shape/local-normal-at :cylinder
  [c point]
  (tup/normalise (tup/vector (tup/x point) 0 (tup/z point))))
