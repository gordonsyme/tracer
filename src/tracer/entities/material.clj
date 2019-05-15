(ns tracer.entities.material
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.colour :as colour]
            [tracer.entities.light :as light]
            [tracer.entities.tuple :as tup]))

(s/def ::non-neg-number (s/and number?
                               (complement neg?)))

(s/def ::colour ::colour/colour)
(s/def ::ambient ::non-neg-number)
(s/def ::diffuse ::non-neg-number)
(s/def ::specular ::non-neg-number)
(s/def ::shininess ::non-neg-number)
(s/def ::material (s/keys :req-un [::colour ::ambient ::diffuse ::specular ::shininess]))

(defn material
  []
  {:colour (colour/colour 1 1 1)
   :ambient 0.1
   :diffuse 0.9
   :specular 0.9
   :shininess 200.0})
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

(defn lighting
  [m light point eye normal]
  (let [effective-colour (colour/hadamard (:colour m) (:intensity light))
        ambient (colour/mul effective-colour (:ambient m))
        lightv (tup/normalise (tup/sub (:position light) point))
        light-dot-normal (tup/dot lightv normal)]
    (if (neg? light-dot-normal)
      ;; light is on the wrong side of the surface, there is no contribution
      ;; from diffuse or specular
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
               :light ::light/light
               :position ::tup/point
               :eye ::tup/vector
               :normal ::tup/vector)
  :ret ::colour/colour)
