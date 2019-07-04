(ns tracer.entities.world
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.colour :as colour]
            [tracer.entities.intersection :as i]
            [tracer.entities.light :as light]
            [tracer.entities.material :as material]
            [tracer.entities.pattern :as pattern]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.tuple :as tup]))

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
  :ret (s/coll-of ::shape/object))

(defn add-object
  [w o]
  (update w :objects conj o))
(s/fdef add-object
  :args (s/cat :w ::world
               :o ::shape/object)
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

(defn shadowed?
  [w light p]
  (let [shadowv (tup/sub (:position light) p)
        shadow-ray (ray/ray p (tup/normalise shadowv))
        hit (i/hit (intersect w shadow-ray))]
    (boolean
      (and hit
           (< (:t hit)
              (tup/magnitude shadowv))))))
(s/fdef shadowed?
  :args (s/cat :w ::world
               :light ::light/light
               :p ::tup/point)
  :ret boolean?)

;; This is so wrong
(declare colour-at)
(defn reflected-colour
  [w comps]
  (let [{:keys [object over-point reflectv ttl]} comps
        material (shape/material object)
        reflectance (:reflective material)]
    (if (or (zero? ttl)
            (zero? reflectance))
      (colour/colour 0 0 0)
      (colour/mul (colour-at w (ray/ray over-point reflectv ttl))
                  reflectance))))
(s/fdef reflected-colour
  :args (s/cat :w ::world
               :comps ::i/computations)
  :ret ::colour/colour)

(defn refracted-colour
  [w comps]
  (let [{:keys [eyev normalv under-point n1 n2 object ttl]} comps
        material (shape/material object)
        transparency (:transparency material)
        n-ratio (/ n1 n2)
        cos-i (tup/dot eyev normalv)
        sin2-t (* (* n-ratio n-ratio)
                  (- 1 (* cos-i cos-i)))]
    (if (or (zero? ttl)
            (zero? transparency)
            (> sin2-t 1))
      (colour/colour 0 0 0)
      (let [cos-t (Math/sqrt (- 1 sin2-t))
            direction (tup/sub
                        (tup/mul normalv (- (* n-ratio cos-i) cos-t))
                        (tup/mul eyev n-ratio))]
        (colour/mul (colour-at w (ray/ray under-point direction ttl))
                    transparency)))))
(s/fdef refracted-colour
  :args (s/cat :w ::world
               :comps ::i/computations)
  :ret ::colour/colour)

(defn- shade-hit
  [w comps]
  (let [{:keys [object over-point eyev normalv]} comps
        material (:material object)
        shader (if-let [pattern (:pattern material)]
                  (partial pattern/colour-at
                           pattern
                           (shape/inverse-transform object))
                  (fn [_point]
                    (:colour material)))
        surface-colour (reduce
                         colour/add
                         (colour/colour 0.0 0.0 0.0)
                         (for [light (lights w)]
                           (material/lighting material
                                              shader
                                              light
                                              over-point
                                              eyev
                                              normalv
                                              (shadowed? w light over-point))))
        reflected-colour (reflected-colour w comps)
        refracted-colour (refracted-colour w comps)]
    (reduce colour/add surface-colour [reflected-colour refracted-colour])))
(s/fdef shade-hit
  :args (s/cat :w ::world
               :comps ::i/computations)
  :ret ::colour/colour)

(defn colour-at
  [w r]
  (let [is (intersect w r)
        hit (i/hit is)]
    (if hit
      (shade-hit w (i/prepare-computations hit r is))
      (colour/colour 0 0 0))))
(s/fdef colour-at
  :args (s/cat :w ::world
               :r ::ray/ray)
  :ret ::colour/colour)
