(ns tracer.entities.intersection
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.ray :as r]
            [tracer.entities.material :as material]
            [tracer.entities.shape :as shape]
            [tracer.entities.tuple :as tup]))

(s/def ::t ::shape/t)

(s/def ::intersection ::shape/intersection)
(s/def ::intersections ::shape/intersections)

(s/def ::eyev ::tup/vector)
(s/def ::normalv ::tup/vector)
(s/def ::reflectv ::tup/vector)
(s/def ::inside boolean?)
(s/def ::over-point ::tup/point)
(s/def ::under-point ::tup/point)
(s/def ::ttl (s/or :zero zero?
                   :pos pos-int?))
(s/def ::computations
  (s/keys :req-un [::t
                   ::shape/object
                   ::tup/point
                   ::over-point
                   ::under-point
                   ::eyev
                   ::normalv
                   ::reflectv
                   ::inside
                   ::ttl]))

(defn intersection
  [t o]
  {:t t
   :object o})
(s/fdef intersection
  :args (s/cat :t ::t
               :o ::shape/object)
  :ret ::intersection)

(defn intersect
  [obj ray]
  (shape/intersect obj ray))
(s/fdef intersect
  :args (s/cat :obj ::shape/object
               :r ::r/ray)
  :ret ::intersections)

(defn intersections
  [& is]
  (vec (sort-by :t is)))
(s/fdef intersections
  :args (s/* ::intersection)
  :ret ::intersections)

(defn hit
  [is]
  (first
    (remove (comp neg? :t) is)))
(s/fdef hit
  :args (s/cat :is ::intersections)
  :ret (s/nilable ::intersection))

(defn- boundary-materials
  ([is] (boundary-materials is (list)))
  ([[i & is] containers]
   (if (nil? i)
     []
     (let [obj (:object i)
           obj-contains-ray? (some #{obj} containers)
           new-containers (remove #{obj} containers)]
       (if obj-contains-ray?
         (lazy-cat
           (take 1 new-containers)
           (boundary-materials is new-containers))
         (lazy-cat
           [obj]
           (boundary-materials is (conj containers obj))))))))

(defn- refraction-boundaries
  [is]
  (let [vacuum (material/material)
        materials (map shape/material (boundary-materials is))]
    (map vector
         (map :t is)
         (map vector
              (concat [vacuum] materials)
              (concat materials [vacuum])))))

(defn refraction-boundary
  [hit is]
  (second
    (first
      (drop-while #(not= (:t hit) (first %))
                  (refraction-boundaries is)))))

(defn prepare-computations
  ([hit r]
   (prepare-computations hit r [hit]))
  ([hit r is]
   (let [{:keys [t object]} hit
         point (r/position r t)
         eye (tup/negate (:direction r))
         normal (shape/normal-at object point)
         inside (neg? (tup/dot normal eye))
         normalv (if inside
                   (tup/negate normal)
                   normal)
         [n1 n2] (if (-> object shape/material material/transparent?)
                   (map material/refractive-index
                        (refraction-boundary hit is))
                   [1 1])]
     {:t t
      :object object
      :point point
      :over-point (tup/add point (tup/mul normalv 0.00000001))
      :under-point (tup/add point (tup/mul normalv -0.00000001))
      :eyev eye
      :normalv normalv
      :reflectv (tup/reflect (r/direction r) normalv)
      :inside inside
      :ttl (dec (r/ttl r))
      :n1 n1
      :n2 n2})))
(s/fdef prepare-computations
  :args (s/alt
          :base (s/cat :i ::intersection
                       :r ::r/ray)
          :intersections (s/cat :i ::intersection
                                :r ::r/ray
                                :is (s/coll-of ::intersection)))
  :ret ::computations)

(defn schlick-reflectance
  [comps]
  ;; This is doing the same work as world/refracted-colour, should try to
  ;; combine, only compute once
  (let [{:keys [eyev normalv n1 n2]} comps
        cos (tup/dot eyev normalv)
        n-ratio (/ n1 n2)
        sin2-t (* (* n-ratio n-ratio)
                  (- 1 (* cos cos)))
        cos (if (> n1 n2)
              (Math/sqrt (- 1 sin2-t))
              cos)]
    (if (and (> n1 n2)
             (> sin2-t 1.0))
      1.0
      (let [root-r0 (/ (- n1 n2)
                       (+ n1 n2))
            r0 (* root-r0 root-r0)]
        (+ r0 (* (- 1 r0)
                 (Math/pow (- 1 cos) 5)))))))
(s/fdef schlick-reflectance
  :args (s/cat :comps ::computations)
  :ret number?)
