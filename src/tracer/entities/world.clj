(ns tracer.entities.world
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.intersection :as i]
            [tracer.entities.light :as light]))

(s/def ::world (s/keys :req-un []))

(defn world
  []
  {:objects #{}
   :lights #{}})
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
