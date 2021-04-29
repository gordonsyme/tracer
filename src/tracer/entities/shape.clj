(ns tracer.entities.shape
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.material :as material]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as ray]
            [tracer.entities.tuple :as tup]))

(s/def ::tag keyword?)

(s/def ::transform ::mat/matrix)
(s/def ::inverse-transform ::mat/matrix)
(s/def ::material ::material/material)
(s/def ::common (s/keys :req [::tag]
                        :req-un [::transform ::inverse-transform ::material]))

(defmulti object-type ::tag)
(s/def ::object (s/multi-spec object-type ::tag))

;; Intersections
(s/def ::t number?)
(s/def ::intersection (s/keys :req-un [::t ::object]))
(s/def ::intersections (s/coll-of ::intersection))

;; Relations between shapes, these need to be defined here because
;; 1. relations need to refer to shape specs
;; 2. `intersect` and `local-intersect` needs to refer to relation specs
(s/def ::parent (s/nilable ::object))
(s/def ::children (s/coll-of ::object))
(s/def ::group-relation (s/keys :req-un [::parent ::children]))
(s/def ::shape-relation (s/keys :req-un [::parent]))

(s/def ::relations (s/map-of ::object (s/or :group ::group-relation
                                            :shape ::shape-relation)))

(defn relations
  []
  {})
(s/fdef relations
  :ret ::relations)

(defn parent
  [relations shape]
  (get-in relations [shape :parent]))
(s/fdef parent
  :args (s/cat :relations ::relations
               :shape ::object)
  :ret (s/nilable ::object))

;; Shape fns
(defn- same-tag?
  [spec-data]
  (= (-> spec-data :ret ::tag)
     (-> spec-data :args :o ::tag)))

(defn transform
  [o]
  (:transform o))
(s/fdef transform
  :args (s/cat :o ::object)
  :ret ::mat/matrix)

(defn inverse-transform
  [o]
  (:inverse-transform o))
(s/fdef inverse-transform
  :args (s/cat :o ::object)
  :ret ::mat/matrix)

(defn with-transform
  [o m]
  (assoc o :transform m
           :inverse-transform (mat/inverse m)))
(s/fdef with-transform
  :args (s/cat :o ::object
               :m ::mat/matrix)
  :fn same-tag?
  :ret ::object)

(defn material
  [o]
  (:material o))
(s/fdef material
  :args (s/cat :o ::object)
  :ret ::material/material)

(defn with-material
  [o m]
  (assoc o :material m))
(s/fdef with-material
  :args (s/cat :o ::object
               :m ::material/material)
  :fn same-tag?
  :ret ::object)

(defmulti local-intersect
  "Intersect a ray with an object.

  `rels` - the relationships between groups and shapes
  `obj` - the shape being intersected with
  `ray` - the ray transformed into *object-space*

  Returns a collection of t values"
  (fn [_rels obj _ray]
    (::tag obj)))
(s/fdef local-intersect
  :args (s/cat :rels ::relations
               :obj ::object
               :ray ::ray/ray)
  :ret ::intersections)

(defn intersect
  [rels obj ray]
  (let [object-space-ray (ray/transform ray (inverse-transform obj))]
    (local-intersect rels obj object-space-ray)))
(s/fdef intersect
  :args (s/cat :rels ::relations
               :obj ::object
               :ray ::ray/ray)
  :ret ::intersections)

(defn world-to-object
  [rels obj point]
  (mat/mult (inverse-transform obj)
            (if-let [p (parent rels obj)]
              (world-to-object rels p point)
              point)))
(s/fdef world-to-object
  :args (s/cat :rels ::relations
               :obj ::object
               :point ::tup/point)
  :ret ::tup/point)

(defn normal-to-world
  [rels obj normal]
  (let [inv-transform (inverse-transform obj)
        transformed-normal (tup/normalise
                             ;; Setting w=0.0 is a hack to account for transposes of translations
                             ;; mucking with the w coordinate of vectors.
                             ;; The correct thing to do is multiply by the inverse of (submatrix
                             ;; transform 3 3) but that reduces 4-component tuples down to 3-component
                             ;; tuples.
                             (assoc (mat/mult (mat/transpose inv-transform) normal)
                                    3
                                    0.0))]
    (if-let [p (parent rels obj)]
      (normal-to-world rels p transformed-normal)
      transformed-normal)))
(s/fdef normal-to-world
  :args (s/cat :rels ::relations
               :obj ::object
               :normal ::tup/vector)
  :ret ::tup/vector)

(defmulti local-normal-at
  "Find the normal at a given point on `obj`, in object-space."
  (fn [obj _point]
    (::tag obj)))
(s/fdef local-normal-at
  :args (s/cat :obj ::object
               :point ::tup/point)
  :ret ::tup/vector)

(defn normal-at
  [rels obj point]
  (->> point
       (world-to-object rels obj)
       (local-normal-at obj)
       (normal-to-world rels obj)))
(s/fdef normal-at
  :args (s/cat :rels ::relations
               :o ::object
               :p ::tup/point)
  :ret ::tup/vector)
