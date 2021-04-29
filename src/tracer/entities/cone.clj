(ns tracer.entities.cone
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.material :as material]
            [tracer.entities.shape :as shape]
            [tracer.entities.ray :as ray]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.math :as math]))

(s/def ::minimum double?)
(s/def ::maximum double?)
(s/def ::closed boolean?)

(s/def ::cone (s/merge ::shape/common
                       (s/keys :req-un [::minimum ::maximum ::closed])))

(defmethod shape/object-type :cone
  [_]
  ::cone)

(defn cone
  []
  {::shape/tag :cone
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)
   :minimum Double/NEGATIVE_INFINITY
   :maximum Double/POSITIVE_INFINITY
   :closed false})
(s/fdef cone
  :ret ::cone)

(defn with-minimum
  [c minimum]
  (assoc c :minimum (double minimum)))
(s/fdef with-minimum
  :args (s/cat :c ::cone
               :minimum number?)
  :ret ::cone)

(defn with-maximum
  [c maximum]
  (assoc c :maximum (double maximum)))
(s/fdef with-maximum
  :args (s/cat :c ::cone
               :maximum number?)
  :ret ::cone)

(defn with-closed
  [c closed]
  (assoc c :closed closed))
(s/fdef with-closed
  :args (s/cat :c ::cone
               :closed boolean?)
  :ret ::cone)

(defn- check-cap
  [r [radius t]]
  (let [[^double origin-x _ ^double origin-z] (ray/origin r)
        [^double dir-x _ ^double dir-z] (ray/direction r)
        x (+ origin-x (* t dir-x))
        z (+ origin-z (* t dir-z))]
    (<= (+ (* x x) (* z z)) (* radius radius))))

(defn- intersect-caps
  [c r]
  (let [^double dir-y (tup/y (ray/direction r))
        ^double origin-y (tup/y (ray/origin r))]
    (if (or (not (:closed c))
            (math/zero? dir-y))
      []
      (let [ts (map
                 (fn [^double bound]
                   [(Math/abs bound)  ;; a cone's radius is the absolute y-value
                    (/ (- bound origin-y)
                       dir-y)])
                 [(:minimum c) (:maximum c)])]
        (->> ts
             (filter (partial check-cap r))
             (map second))))))

(defn- local-intersect
  [cone ray]
  (let [origin (ray/origin ray)
        direction (ray/direction ray)
        dir-x (tup/x direction)
        dir-y (tup/y direction)
        dir-z (tup/z direction)
        origin-x (tup/x origin)
        origin-y (tup/y origin)
        origin-z (tup/z origin)
        a (+ (* dir-x dir-x)
             (- (* dir-y dir-y))
             (* dir-z dir-z))
        b (+ (* 2 origin-x dir-x)
             (- (* 2 origin-y dir-y))
             (* 2 origin-z dir-z))
        c (+ (* origin-x origin-x)
             (- (* origin-y origin-y))
             (* origin-z origin-z))]
    (cond
      ;; Ray misses
      (and (math/zero? a)
           (math/zero? b))
      []

      ;; Ray parallel to one of the cone's halves
      (math/zero? a)
      (sort
        (concat
          (intersect-caps cone ray)
          [(- (/ c (* 2 b)))]))

      :else
      (let [discriminant (- (* b b)
                            (* 4 a c))
            ts (if (neg? discriminant)
                 []
                 (let [root (Math/sqrt discriminant)]
                   [(/ (- (- b) root)
                       (* 2 a))
                    (/ (+ (- b) root)
                       (* 2 a))]))]
        (sort
          (concat
            (intersect-caps cone ray)
            (filter (fn [t]
                      (< (:minimum cone)
                         (+ (tup/y origin)
                            (* t (tup/y direction)))
                         (:maximum cone)))
                    ts)))))))
(s/fdef local-intersect
  :args (s/cat :cone ::cone
               :ray ::ray/ray)
  :ret (s/coll-of ::shape/t))

(defmethod shape/local-intersect :cone
  [_rels cone ray]
  (map (partial hash-map :object cone :t)
       (local-intersect cone ray)))

(defmethod shape/local-normal-at :cone
  [c point]
  (let [[^double x ^double y ^double z] point
        dist (+ (* x x) (* z z))]
    (cond
      (and (< dist 1)
           (>= y (- (:maximum c) math/EPSILON)))
      (tup/vector 0 1 0)

      (and (< dist 1)
           (<= y (+ (:minimum c) math/EPSILON)))
      (tup/vector 0 -1 0)

      :else
      (let [normal-y (if (> y 0)
                       (- (Math/sqrt dist))
                       (Math/sqrt dist))]
        (tup/vector x normal-y z)))))
