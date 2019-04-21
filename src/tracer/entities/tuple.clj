(ns tracer.entities.tuple
  (:refer-clojure :exclude [vector? vector])
  (:require [clojure.spec.alpha :as s]))

(s/def ::tuple (s/and (s/coll-of number? :count 4)
                      clojure.core/vector?))

(defn tuple
  "Create a tuple, a 4-element vector of numbers"
  [x y z w]
  [(double x) (double y) (double z) (double w)])

(s/fdef tuple
  :args (s/cat :x number? :y number? :z number? :w number?)
  :ret ::tuple)

(defn x
  [a1]
  (first a1))
(s/fdef x
  :args (s/cat :a1 ::tuple)
  :ret double?)

(defn y
  [a1]
  (second a1))
(s/fdef y
  :args (s/cat :a1 ::tuple)
  :ret double?)

(defn z
  [a1]
  (nth a1 2))
(s/fdef z
  :args (s/cat :a1 ::tuple)
  :ret double?)

(defn w
  [a1]
  (nth a1 3))
(s/fdef w
  :args (s/cat :a1 ::tuple)
  :ret double?)

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

(defn add
  "Add two tuples"
  [a1 a2]
  (mapv + a1 a2))

(s/fdef add
  :args (s/or :vector-point (s/cat :a1 ::vector :a2 ::point)
              :vector-vector (s/cat :a1 ::vector :a2 ::vector)
              :point-vector (s/cat :a1 ::point :a2 ::vector))
  :ret (s/or :vector ::vector
             :point ::point))

(defn sub
  "Subtract two tuples"
  [a1 a2]
  (mapv - a1 a2))

(s/fdef sub
  :args (s/or :vector-vector (s/cat :a1 ::vector :a2 ::vector)
              :point-point (s/cat :a1 ::point :a2 ::point)
              :point-vector (s/cat :a1 ::point :a2 ::vector))
  :ret (s/or :vector ::vector
             :point ::point))

(defn negate
  "Negate a tuple"
  [a1]
  (mapv - a1))

(s/fdef negate
  :args (s/cat :a1 ::tuple)
  :ret ::tuple)

(defn mul
  "Multiply a tuple by a scalar"
  [a1 s]
  (mapv #(* % s) a1))

(s/fdef mul
  :args (s/cat :a1 ::tuple :s number?)
  :ret ::tuple)

(defn div
  "Divide a tuple by a scalar."
  [a1 s]
  (let [s (double s)]
    (mapv #(/ % s) a1)))

(s/fdef div
  :args (s/cat :a1 ::tuple :s number?)
  :ret ::tuple)

(defn magnitude
  "Compute the magnitude of a vector"
  [v]
  (Math/sqrt (reduce + (map #(* % %) v))))

(s/fdef magnitude
  :args (s/cat :v ::vector)
  :ret double?)

(defn normalise
  [v]
  (let [m (magnitude v)]
    (mapv #(/ % m) v)))

(s/fdef normalise
  :args (s/cat :v ::vector)
  :ret ::vector)

(defn dot
  [v1 v2]
  (reduce + (map * v1 v2)))

(s/fdef dot
  :args (s/cat :v1 ::vector
               :v2 ::vector)
  :ret double?)

(defn cross
  [v1 v2]
  (vector (- (* (y v1) (z v2))
             (* (z v1) (y v2)))
          (- (* (z v1) (x v2))
             (* (x v1) (z v2)))
          (- (* (x v1) (y v2))
             (* (y v1) (x v2)))))

(s/fdef cross
  :args (s/cat :v1 ::vector
               :v2 ::vector)
  :ret ::vector)
