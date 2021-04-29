(ns tracer.entities.cube
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.material :as material]
            [tracer.entities.ray :as ray]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(s/def ::cube ::shape/common)

(defmethod shape/object-type :cube
  [_]
  ::cube)

(defn cube
  []
  {::shape/tag :cube
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)})
(s/fdef cube
  :ret ::cube)

(defn- check-axis
  [origin direction]
  (let [tmin-numerator (- -1 ^double origin)
        tmax-numerator (-  1 ^double origin)
        ts (if (>= (Math/abs ^double direction)
                   0.00001)
             [(/ tmin-numerator ^double direction)
              (/ tmax-numerator ^double direction)]
             [(* tmin-numerator Double/POSITIVE_INFINITY)
              (* tmax-numerator Double/POSITIVE_INFINITY)])]
    (sort ts)))
(s/fdef check-axis
  :args (s/cat :origin double?
               :direction double?)
  :ret (s/coll-of double? :count 2))

(defn- local-intersect
  [ray]
  ;; returns a list of pairs, need max of first, min of second
  (let [ts (map (fn [axis]
                  (check-axis (-> ray ray/origin axis)
                              (-> ray ray/direction axis)))
                [tup/x tup/y tup/z])
        tmin (apply max (map first ts))
        tmax (apply min (map second ts))]
    (if (> tmin tmax)
      []
      [tmin tmax])))
(s/fdef local-intersect
  :args (s/cat :ray ::ray/ray)
  :ret (s/coll-of ::shape/t))

(defmethod shape/local-intersect :cube
  [_rels c ray]
  (map (partial hash-map :object c :t)
       (local-intersect ray)))

(defmethod shape/local-normal-at :cube
  [_c point]
  (let [[x y z :as coords] (take 3 point)
        maxc (apply max (map (fn [^double coord]
                               (Math/abs coord))
                             coords))]
    (cond
      (= maxc (Math/abs ^double x)) (tup/vector x 0 0)
      (= maxc (Math/abs ^double y)) (tup/vector 0 y 0)
      :else (tup/vector 0 0 z))))
