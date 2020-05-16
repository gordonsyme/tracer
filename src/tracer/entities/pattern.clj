(ns tracer.entities.pattern
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.colour :as colour]
            [tracer.entities.matrix :as mat]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(s/def ::transform ::mat/matrix)
(s/def ::inverse-transform ::mat/matrix)
(s/def ::shader (s/fspec :args (s/cat :point ::tup/point)
                         :ret ::colour/colour))

(s/def ::pattern (s/keys :req-un [::transform ::inverse-transform ::shader]))

(defn pattern
  [shader]
  {:transform (transform/identity)
   :inverse-transform (transform/identity)
   :shader shader})
(s/fdef pattern
  :args (s/cat :shader ::shader)
  :ret ::pattern)

(defn with-transform
  [p m]
  (assoc p :transform m
           :inverse-transform (mat/inverse m)))
(s/fdef with-transform
  :args (s/cat :p ::pattern
               :m ::mat/matrix)
  :ret ::pattern)

(defn- local-colour-at
  [pattern point]
  ((:shader pattern) (mat/mult (:inverse-transform pattern) point)))
(s/fdef local-colour-at
  :args (s/cat :pattern ::pattern
               :point ::tup/point)
  :ret ::colour/colour)

(defn colour-at
  [pattern object-transform point]
  (local-colour-at pattern (mat/mult object-transform point)))
(s/fdef colour-at
  :args (s/cat :pattern ::pattern
               :object ::mat/matrix
               :point ::tup/point)
  :ret ::colour/colour)

(defn stripe-pattern
  [a b]
  (pattern
    (fn [point]
      (if (zero? (mod (Math/floor (tup/x point)) 2))
        (local-colour-at a point)
        (local-colour-at b point)))))
(s/fdef stripe-pattern
  :args (s/cat :a ::pattern
               :b ::pattern)
  :ret ::pattern)

(defn colour-pattern
  [c]
  (pattern
    (fn [_point]
      c)))
(s/fdef colour-pattern
  :args (s/cat :c ::colour/colour)
  :ret ::pattern)

(defn stripes
  "Helper fn to build a two-colour striped pattern"
  [c1 c2]
  (stripe-pattern (colour-pattern c1) (colour-pattern c2)))
(s/fdef stripes
  :args (s/cat :c1 ::colour/colour
               :c2 ::colour/colour)
  :ret ::pattern)

(defn gradient-pattern
  [a b]
  (pattern
    (fn [point]
      (let [ca (local-colour-at a point)
            cb (local-colour-at b point)
            distance (colour/sub cb ca)
            fraction (- (tup/x point)
                        (Math/floor (tup/x point)))]
        (colour/add ca
                    (colour/mul distance fraction))))))
(s/fdef gradient-pattern
  :args (s/cat :a ::pattern
               :b ::pattern)
  :ret ::pattern)

(defn gradient
  "Helper fn to build a two-colour gradient pattern"
  [c1 c2]
  (gradient-pattern (colour-pattern c1) (colour-pattern c2)))
(s/fdef gradient
  :args (s/cat :c1 ::colour/colour
               :c2 ::colour/colour)
  :ret ::pattern)

(defn ring-pattern
  [a b]
  (pattern
    (fn [point]
      (let [distance (Math/floor
                       (Math/sqrt
                         (+ (* (tup/x point)
                               (tup/x point))
                            (* (tup/z point)
                               (tup/z point)))))]
        (if (zero? (mod distance 2))
          (local-colour-at a point)
          (local-colour-at b point))))))
(s/fdef ring-pattern
  :args (s/cat :a ::pattern
               :b ::pattern)
  :ret ::pattern)

(defn rings
  "Helper fn to build a two-colour ring pattern"
  [c1 c2]
  (ring-pattern (colour-pattern c1) (colour-pattern c2)))
(s/fdef rings
  :args (s/cat :c1 ::colour/colour
               :c2 ::colour/colour)
  :ret ::pattern)

(defn checked-pattern
  [a b]
  (pattern
    (fn [point]
      (let [distance (+ (Math/floor (tup/x point))
                        (Math/floor (tup/y point))
                        (Math/floor (tup/z point)))]
        (if (zero? (mod distance 2))
          (local-colour-at a point)
          (local-colour-at b point))))))
(s/fdef checked-pattern
  :args (s/cat :a ::pattern
               :b ::pattern)
  :ret ::pattern)

(defn checks
  "Helper fn to build a two-colour 3d checked pattern"
  [c1 c2]
  (checked-pattern (colour-pattern c1) (colour-pattern c2)))
(s/fdef checks
  :args (s/cat :c1 ::colour/colour
               :c2 ::colour/colour)
  :ret ::pattern)

(defn blended-pattern
  [a b]
  (pattern
    (fn [point]
      (colour/blend (local-colour-at a point)
                    (local-colour-at b point)))))
(s/fdef blended-pattern
  :args (s/cat :a ::pattern
               :b ::pattern)
  :ret ::pattern)
