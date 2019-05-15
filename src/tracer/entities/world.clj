(ns tracer.entities.world
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.colour :as colour]
            [tracer.entities.intersection :as i]
            [tracer.entities.light :as light]
            [tracer.entities.material :as material]
            [tracer.entities.ray :as ray]))

(s/def ::world (s/keys :req-un []))

(defn world
  []
  {:objects []
   :lights []})
(s/fdef world
  :ret ::world)

(defn objects
  [w]
  (:objects w))
(s/fdef objects
  :args (s/cat :w ::world)
  :ret (s/coll-of ::i/object))

(defn add-object
  [w o]
  (update w :objects conj o))
(s/fdef add-object
  :args (s/cat :w ::world
               :o ::i/object)
  :ret ::world)

(defn lights
  [w]
  (:lights w))
(s/fdef lights
  :args (s/cat :w ::world)
  :ret (s/coll-of ::light/light))

(defn add-light
  [w l]
  (update w :lights conj l))
(s/fdef add-light
  :args (s/cat :w ::world
               :l ::light/light)
  :ret ::world)

(defn intersect
  [w r]
  (apply i/intersections (mapcat #(i/intersect % r) (objects w))))
(s/fdef intersect
  :args (s/cat :w ::world
               :r ::ray/ray)
  :ret ::i/intersections)

(defn shade-hit
  [w comps]
  (reduce
    colour/add
    (colour/colour 0.0 0.0 0.0)
    (for [light (lights w)]
      (material/lighting (:material (:object comps))
                         light
                         (:point comps)
                         (:eyev comps)
                         (:normalv comps)))))
(s/fdef shade-hit
  :args (s/cat :w ::world
               :comps ::i/computations)
  :ret ::colour/colour)
