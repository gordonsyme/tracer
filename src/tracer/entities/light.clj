(ns tracer.entities.light
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.colour :as colour]
            [tracer.entities.tuple :as tup]))

(s/def ::position ::tup/point)
(s/def ::intensity ::colour/colour)
(s/def ::light (s/keys :req-un [::position ::intensity]))

(defn point-light
  [pos intensity]
  {:position pos
   :intensity intensity})
(s/fdef point-light
  :args (s/cat :pos ::position
               :intensity ::intensity)
  :ret ::light)
