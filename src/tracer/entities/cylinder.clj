(ns tracer.entities.cylinder
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.material :as material]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.math :refer (EPSILON)]))

(s/def ::minimum double?)
(s/def ::maximum double?)
(s/def ::closed boolean?)

(s/def ::cylinder (s/merge ::shape/common
                           (s/keys :req-un [::minimum ::maximum ::closed])))

(defmethod shape/object-type :cylinder
  [_]
  ::cylinder)

(defn cylinder
  []
  {::shape/tag :cylinder
   ::shape/id (java.util.UUID/randomUUID)
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)
   :minimum Double/NEGATIVE_INFINITY
   :maximum Double/POSITIVE_INFINITY
   :closed false})
(s/fdef cylinder
  :ret ::cylinder)

(defn with-minimum
  [c minimum]
  (assoc c :minimum (double minimum)))
(s/fdef with-minimum
  :args (s/cat :c ::cylinder
               :minimum number?)
  :ret ::cylinder)

(defn with-maximum
  [c maximum]
  (assoc c :maximum (double maximum)))
(s/fdef with-maximum
  :args (s/cat :c ::cylinder
               :maximum number?)
  :ret ::cylinder)

(defn with-closed
  [c closed]
  (assoc c :closed closed))
(s/fdef with-closed
  :args (s/cat :c ::cylinder
               :closed boolean?)
  :ret ::cylinder)

(defn- check-cap
  [r t]
  (let [[^double origin-x _ ^double origin-z] (ray/origin r)
        [^double dir-x _ ^double dir-z] (ray/direction r)
        x (+ origin-x (* t dir-x))
        z (+ origin-z (* t dir-z))]
    (<= (+ (* x x) (* z z)) 1)))

(defn- intersect-caps
  [c r]
  (let [^double dir-y (tup/y (ray/direction r))
        ^double origin-y (tup/y (ray/origin r))]
    (if (or (not (:closed c))
            (< (Math/abs dir-y) EPSILON))
      []
      (let [ts (map
                 (fn [^double bound]
                   (/ (- bound origin-y)
                      dir-y))
                 [(:minimum c) (:maximum c)])]
        (filter (partial check-cap r) ts)))))

(defn- local-intersect
  [cyl ray]
  (let [origin (ray/origin ray)
        direction (ray/direction ray)
        dir-x (tup/x direction)
        dir-z (tup/z direction)
        a (+ (* dir-x dir-x)
             (* dir-z dir-z))]
    (if (< (Math/abs ^double a) EPSILON)
      (intersect-caps cyl ray)
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
        (sort
          (concat
            (intersect-caps cyl ray)
            (filter (fn [t]
                      (< (:minimum cyl)
                         (+ (tup/y origin)
                            (* t (tup/y direction)))
                         (:maximum cyl)))
                    ts)))))))
(s/fdef local-intersect
  :args (s/cat :cylinder ::cylinder
               :ray ::ray/ray)
  :ret (s/coll-of ::shape/t))

(defmethod shape/local-intersect :cylinder
  [_rels cyl ray]
  (map (partial hash-map :object cyl :t)
       (local-intersect cyl ray)))

(defmethod shape/local-normal-at :cylinder
  [c point]
  (let [[^double x ^double y ^double z] point
        dist (+ (* x x) (* z z))]
    (cond
      (and (< dist 1)
           (>= y (- (:maximum c) EPSILON)))
      (tup/vector 0 1 0)

      (and (< dist 1)
           (<= y (+ (:minimum c) EPSILON)))
      (tup/vector 0 -1 0)

      :else
      (tup/vector x 0 z))))
