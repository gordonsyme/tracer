(ns tracer.entities.colour
  (:require [clojure.spec.alpha :as s]))

(s/def ::colour (s/and (s/coll-of number? :count 3)
                       vector?))

(defn colour
  [r g b]
  [(double r) (double g) (double b)])
(s/fdef colour
  :args (s/cat :r number? :g number? :b number?)
  :ret ::colour)

(defn red
  [c]
  (first c))
(s/fdef red
  :args (s/cat :c ::colour)
  :ret number?)

(defn green
  [c]
  (second c))
(s/fdef green
  :args (s/cat :c ::colour)
  :ret number?)

(defn blue
  [c]
  (nth c 2))
(s/fdef blue
  :args (s/cat :c ::colour)
  :ret number?)

(defn add
  [c1 c2]
  (mapv + c1 c2))
(s/fdef add
  :args (s/cat :c1 ::colour
               :c2 ::colour)
  :ret ::colour)

(defn sub
  [c1 c2]
  (mapv - c1 c2))
(s/fdef sub
  :args (s/cat :c1 ::colour
               :c2 ::colour)
  :ret ::colour)

(defn mul
  [c1 s]
  (mapv #(* % s) c1))
(s/fdef mul
  :args (s/cat :c1 ::colour
               :s number?)
  :ret ::colour)

(defn hadamard
  [c1 c2]
  (mapv * c1 c2))
(s/fdef hadamard
  :args (s/cat :c1 ::colour
               :c2 ::colour)
  :ret ::colour)

(defn blend
  [c1 c2]
  (mapv #(/ % 2) (map + c1 c2)))
(s/fdef blend
  :args (s/cat :c1 ::colour
               :c2 ::colour)
  :ret ::colour)
