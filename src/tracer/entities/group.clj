(ns tracer.entities.group
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.intersection :as i]
            [tracer.entities.material :as material]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(s/def ::group ::shape/common)

(defmethod shape/object-type :group
  [_]
  ::group)

(defn group
  []
  {::shape/tag :group
   ::shape/id (java.util.UUID/randomUUID)
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)})
(s/fdef group
  :ret ::group)

(defn- update-bounds
  "Return a new ::shape/bounds bounding box, updated to include `shape`.

  NB `shape`'s bounds are in object space, the returned bounds are in group space."
  [rels bounds shape]
  ;; To convert from object space to group-space multiply by the object's
  ;; transform
  ;; Use all corners of the transformed cube to work out new bounds
  ;; This will be a `reduce` style op
  ;; Group bounds should be recomputed when objects are added to the group, not
  ;; on every ray intersect test.
  ;; Group bounds should be stored in `rels`!
  (let [{[min-x min-y min-z _] :min-bound
         [max-x max-y max-z _] :max-bound} (shape/bounds rels shape)
        corners (map (partial mat/mult (:transform shape))
                     [(tup/point min-x min-y min-z)
                      (tup/point min-x min-y max-z)
                      (tup/point min-x max-y min-z)
                      (tup/point min-x max-y max-z)
                      (tup/point max-x min-y min-z)
                      (tup/point max-x min-y max-z)
                      (tup/point max-x max-y min-z)
                      (tup/point max-x max-y max-z)])]
    {:min-bound (tup/point (apply min (tup/x (:min-bound bounds)) (map tup/x corners))
                           (apply min (tup/y (:min-bound bounds)) (map tup/y corners))
                           (apply min (tup/z (:min-bound bounds)) (map tup/z corners)))
     :max-bound (tup/point (apply max (tup/x (:max-bound bounds)) (map tup/x corners))
                           (apply max (tup/y (:max-bound bounds)) (map tup/y corners))
                           (apply max (tup/z (:max-bound bounds)) (map tup/z corners)))}))
(s/fdef update-bounds
  :args (s/cat :rels ::shape/relations
               :bounds ::shape/bounds
               :shape ::shape/object)
  :ret ::shape/bounds)

(defn add-child
  [relations group shape]
  (let [new-bounds (update-bounds relations (shape/bounds relations group) shape)]
    (-> relations
        (update-in [group :children] (fnil conj []) shape)
        (update-in [group :parent] identity)
        (assoc-in [group :bounds] new-bounds)
        (assoc-in [shape :parent] group))))
(s/fdef add-child
  :args (s/cat :relations ::shape/relations
               :group ::group
               :shape ::shape/object)
  :ret ::shape/relations)

(defn add-children
  [relations group shapes]
  (reduce (fn [acc obj]
            (add-child acc group obj))
          relations
          shapes))
(s/fdef add-children
  :args (s/cat :relations ::shape/relations
               :group ::group
               :shape (s/coll-of ::shape/object))
  :ret ::shape/relations)

(defn children
  [relations group]
  (if-let [cs (get-in relations [group :children])]
    cs
    []))
(s/fdef children
  :args (s/cat :relations ::shape/relations
               :group ::group)
  :ret ::shape/children)

(defn- check-bounds-axis
  [bound-min bound-max origin direction]
  (let [tmin-numerator (- bound-min ^double origin)
        tmax-numerator (- bound-max ^double origin)
        ts (if (>= (Math/abs ^double direction)
                   0.00001)
             [(/ tmin-numerator ^double direction)
              (/ tmax-numerator ^double direction)]
             [(* tmin-numerator Double/POSITIVE_INFINITY)
              (* tmax-numerator Double/POSITIVE_INFINITY)])]
    (sort ts)))
(s/fdef check-bounds-axis
  :args (s/cat :bound-min double?
               :bound-max double?
               :origin double?
               :direction double?)
  :ret (s/coll-of double? :count 2))

(defn- intersects-bounds?
  [rels g ray]
  ;; returns a list of pairs, need max of first, min of second
  (let [ts (map (fn [axis]
                  (let [{:keys [min-bound max-bound]} (shape/bounds rels g)]
                    (check-bounds-axis
                      (axis min-bound)
                      (axis max-bound)
                      (-> ray ray/origin axis)
                      (-> ray ray/direction axis))))
                [tup/x tup/y tup/z])
        tmin (apply max (map first ts))
        tmax (apply min (map second ts))]
    (<= tmin tmax)))
(s/fdef intersects-bounds?
  :args (s/cat :rels ::shape/relations
               :g ::group
               :ray ::ray/ray)
  :ret boolean?)

(defmethod shape/local-intersect :group
  [rels g ray]
  (if (intersects-bounds? rels g ray)
    (->> (children rels g)
         (mapcat #(i/intersect rels % ray))
         (sort-by :t))
    []))

(defmethod shape/bounds :group
  [rels g]
  (get-in rels [g :bounds] {:min-bound (tup/point 0 0 0)
                            :max-bound (tup/point 0 0 0)}))
