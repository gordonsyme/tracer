(ns tracer.entities.material
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.pattern :as pattern]
            [tracer.entities.tuple :as tup]))

(s/def ::non-neg-number (s/and number?
                               (complement neg?)))

(s/def ::colour ::colour/colour)
(s/def ::ambient ::non-neg-number)
(s/def ::diffuse ::non-neg-number)
(s/def ::specular ::non-neg-number)
(s/def ::shininess ::non-neg-number)
(s/def ::reflective ::non-neg-number)
(s/def ::pattern (s/nilable ::pattern/pattern))
(s/def ::transparency ::non-neg-number)
(s/def ::refractive-index ::non-neg-number)
(s/def ::material
  (s/keys :req-un [::colour
                   ::ambient
                   ::diffuse
                   ::specular
                   ::shininess
                   ::reflective
                   ::pattern
                   ::transparency
                   ::refractive-index]))

(defn material
  []
  {:colour (colour/colour 1 1 1)
   :ambient 0.1
   :diffuse 0.9
   :specular 0.9
   :shininess 200.0
   :reflective 0.0
   :transparency 0.0
   :refractive-index 1.0
   :pattern nil})
(s/fdef material
  :ret ::material)

(defn with-colour
  [m c]
  (assoc m :colour c))
(s/fdef with-colour
  :args (s/cat :m ::material
               :c ::colour/colour)
  :ret ::material)

(defn with-ambient
  [m ambient]
  (assoc m :ambient ambient))
(s/fdef with-ambient
  :args (s/cat :m ::material
               :ambient ::non-neg-number)
  :ret ::material)

(defn with-diffuse
  [m diffuse]
  (assoc m :diffuse diffuse))
(s/fdef with-diffuse
  :args (s/cat :m ::material
               :diffuse ::non-neg-number)
  :ret ::material)

(defn with-specular
  [m specular]
  (assoc m :specular specular))
(s/fdef with-specular
  :args (s/cat :m ::material
               :specular ::non-neg-number)
  :ret ::material)

(defn with-shininess
  [m shininess]
  (assoc m :shininess shininess))
(s/fdef with-shininess
  :args (s/cat :m ::material
               :shininess ::non-neg-number)
  :ret ::material)

(defn with-reflective
  [m reflective]
  (assoc m :reflective reflective))
(s/fdef with-reflective
  :args (s/cat :m ::material
               :reflective ::reflective)
  :ret ::material)

(defn with-pattern
  [m pattern]
  (assoc m :pattern pattern))
(s/fdef with-pattern
  :args (s/cat :m ::material
               :pattern ::pattern/pattern)
  :ret ::material)

(defn with-transparency
  [m transparency]
  (assoc m :transparency transparency))
(s/fdef with-transparency
  :args (s/cat :m ::material
               :transparency ::transparency)
  :ret ::material)

(defn transparent?
  [m]
  (not (zero? (:transparency m))))
(s/fdef transparent?
  :args (s/cat :m ::material)
  :ret boolean?)

(defn with-refractive-index
  [m refractive-index]
  (assoc m :refractive-index refractive-index))
(s/fdef with-refractive-index
  :args (s/cat :m ::material
               :refractive-index ::refractive-index)
  :ret ::material)

(defn refractive-index
  [m]
  (:refractive-index m))
(s/fdef refractive-index
  :args (s/cat :m ::material)
  :ret ::refractive-index)

(defn lighting
  [m shader light point eye normal in-shadow?]
  (let [colour (shader point)
        effective-colour (colour/hadamard colour (:intensity light))
        ambient (colour/mul effective-colour (:ambient m))
        lightv (tup/normalise (tup/sub (:position light) point))
        light-dot-normal (tup/dot lightv normal)]
    (if (or in-shadow?
            (neg? light-dot-normal))
      ;; light is on the wrong side of the surface when the dot product of the
      ;; light vector and normal vector is negative, there is no contribution
      ;; from diffuse or specular
      ;;
      ;; TODO when a point is in shadow the amount of the shadow should be
      ;; determined by the transparency of the object shadowing it, that gets
      ;; much harder when there are multiple objects in the path between the
      ;; point being coloured and the light-source.
      ;; As an approximation, multiplying the transparencies of the objects and
      ;; using that to weight the amount of light that gets through may be
      ;; reasonable.
      ;; E.g. two objects with transparency 0.5 and 0.7 reduce the amount of
      ;; light by half, and then by a further seven tenths.
      ;; So 0.5 * 0.7 == 0.35, which means about 35% of the light gets through,
      ;; so the final colour is reduced to 35% of its intensity.
      ;; This does not account for the thickness of the material, which clearly
      ;; matters.
      ambient
      (let [diffuse (colour/mul effective-colour (* (:diffuse m) light-dot-normal))
            reflectv (tup/reflect (tup/negate lightv) normal)
            reflect-dot-eye (tup/dot reflectv eye)
            specular (if (not (pos? reflect-dot-eye))
                       (colour/colour 0 0 0)
                       (colour/mul (:intensity light)
                                   (* (:specular m)
                                      (Math/pow reflect-dot-eye (:shininess m)))))]
        (-> ambient
            (colour/add diffuse)
            (colour/add specular))))))
(s/fdef lighting
  :args (s/cat :m ::material
               :shader (s/fspec :args (s/cat :point ::tup/point)
                                :ret ::colour/colour)
               :light ::light/light
               :position ::tup/point
               :eye ::tup/vector
               :normal ::tup/vector
               :in-shadow? boolean?)
  :ret ::colour/colour)


(defn vacuum
  []
  (-> (material)
      (with-refractive-index 1.0)
      (with-transparency 0.95)))
(s/fdef vacuum
  :ret ::material)

(defn air
  []
  (-> (material)
      (with-refractive-index 1.00029)
      (with-transparency 0.95)
      (with-reflective 0.95)))
(s/fdef air
  :ret ::material)

(defn water
  []
  (-> (material)
      (with-refractive-index 1.333)
      (with-transparency 0.95)
      (with-reflective 0.95)))
(s/fdef water
  :ret ::material)

(defn glass
  []
  (-> (material)
      (with-refractive-index 1.52)
      (with-transparency 0.95)
      (with-reflective 0.95)))
(s/fdef glass
  :ret ::material)

(defn diamond
  []
  (-> (material)
      (with-refractive-index 2.417)
      (with-transparency 0.95)
      (with-reflective 0.95)))
(s/fdef diamond
  :ret ::material)
