(ns tracer.entities.ray
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.matrix :as mat]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as t]))

(s/def ::origin ::t/point)
(s/def ::direction ::t/vector)
(s/def ::ttl (s/or :zero zero?
                   :pos pos-int?))
(s/def ::ray (s/keys :req-un [::origin ::direction ::ttl]))

(defn ray
  ([o d]
   {:origin o
    :direction d
    :ttl 4})
  ([o d ttl]
   {:origin o
    :direction d
    :ttl ttl}))
(s/fdef ray
  :args (s/alt
          :ray (s/cat :o ::origin
                      :d ::direction)
          :with-ttl (s/cat :o ::origin
                           :d ::direction
                           :ttl pos-int?))
  :ret ::ray)

(defn origin
  [r]
  (:origin r))
(s/fdef origin
  :args (s/cat :r ::ray)
  :ret ::origin)

(defn direction
  [r]
  (:direction r))
(s/fdef direction
  :args (s/cat :r ::ray)
  :ret ::direction)

(defn ttl
  [r]
  (:ttl r))
(s/fdef ttl
  :args (s/cat :r ::ray)
  :ret ::ttl)

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
   :direction (transform/apply m (:direction r))
   :ttl (:ttl r)})
(s/fdef transform
  :args (s/cat :r ::ray
               :m ::mat/matrix)
  :ret ::ray)
