(ns tracer.entities.intersection
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.material :as material]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as r]
            [tracer.entities.tuple :as tup]))

(s/def ::tag keyword?)

(s/def ::transform ::mat/matrix)
(s/def ::inverse-transform ::mat/matrix)
(s/def ::material ::material/material)
(s/def ::object-common (s/keys :req [::tag]
                               :req-un [::transform ::inverse-transform ::material]))

(defmulti object-type ::tag)
(s/def ::object (s/multi-spec object-type ::tag))
(s/def ::t number?)

(s/def ::intersection (s/keys :req-un [::t ::object]))
(s/def ::intersections (s/coll-of ::intersection))

(defn intersection
  [t o]
  {:t t
   :object o})
(s/fdef intersection
  :args (s/cat :t ::t
               :o ::object)
  :ret ::intersection)

(defmulti intersect
  (fn [obj _ray]
    (::tag obj)))
(s/fdef intersect
  :args (s/cat :obj ::object
               :r ::r/ray)
  :ret ::intersections)

(defmulti normal-at
  (fn [obj _point]
    (::tag obj)))
(s/fdef normal-at
  :args (s/cat :o ::object
               :p ::tup/point)
  :ret ::tup/vector)

(defn intersections
  [& is]
  (vec (sort-by :t is)))
(s/fdef intersections
  :args (s/* ::intersection)
  :ret ::intersections)

(defn hit
  [is]
  (first
    (remove (comp neg? :t) is)))
(s/fdef hit
  :args (s/cat :is ::intersections)
  :ret (s/nilable ::intersection))
