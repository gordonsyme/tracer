(ns tracer.entities.ray
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.matrix :as mat]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as t]))

(s/def ::origin ::t/point)
(s/def ::direction ::t/vector)
(s/def ::ray (s/keys :req-un [::origin ::direction]))

(defn ray
  [o d]
  {:origin o
   :direction d})
(s/fdef ray
  :args (s/cat :o ::origin
               :d ::direction)
  :ret ::ray)

(defn position
  [r t]
  (t/add (:origin r)
         (t/mul (:direction r) t)))
(s/fdef position
  :args (s/cat :r ::ray
               :t number?)
  :ret ::t/point)

(defn transform
  [r m]
  {:origin (transform/apply m (:origin r))
   :direction (transform/apply m (:direction r))})
(s/fdef transform
  :args (s/cat :r ::ray
               :m ::mat/matrix)
  :ret ::ray)
