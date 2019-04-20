(ns tracer.entities.tuple
  (:refer-clojure :exclude [vector?])
  (:require [clojure.spec.alpha :as s]))

(s/def ::tuple (s/coll-of double? :count 4))

(defn- tuple
  "Create a tuple, a 4-element vector of numbers"
  [x y z w]
  [(double x) (double y) (double z) (double w)])

(s/fdef tuple
  :args (s/cat :x number? :y number? :z number? :w number?)
  :ret ::tuple)

(defn add
  "Add two tuples"
  [a1 a2]
  (mapv + a1 a2))

(s/fdef add
  :args (s/cat :a1 ::tuple :a2 ::tuple)
  :ret ::tuple)

(defn point?
  "Returns true if a1 is a tuple with w=1.0"
  [a1]
  (= 1.0 (double (nth a1 3))))

(s/fdef point?
  :args (s/cat :a1 ::tuple)
  :ret boolean?)

(s/def ::point (s/and ::tuple
                      point?))

(defn point
  "Create a point, a tuple with w=1.0"
  [x y z]
  (tuple x y z 1.0))

(s/fdef point
  :args (s/cat :x number? :y number? :z number?)
  :ret ::point)

(defn vector?
  "Returns true if a1 is a tuple with w=0.0"
  [a1]
  (zero? (nth a1 3)))

(s/fdef vector?
  :args (s/cat :a1 ::tuple)
  :ret boolean?)

(s/def ::vector (s/and ::tuple
                       vector?))

(defn vector
  "Create a vector, a tuple with w=0.0"
  [x y z]
  (tuple x y z 0.0))

(s/fdef vector
  :args (s/cat :x number? :y number? :z number?)
  :ret ::vector)
