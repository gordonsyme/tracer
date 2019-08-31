(ns tracer.entities.transform
  (:refer-clojure :exclude [apply identity])
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.matrix :as mat]
            [tracer.entities.tuple :as tup]))

(defn translation
  [dx dy dz]
  (mat/matrix [1 0 0 dx]
              [0 1 0 dy]
              [0 0 1 dz]
              [0 0 0  1]))
(s/fdef translation
  :args (s/cat :dx number?
               :dy number?
               :dz number?)
  :ret ::mat/matrix)

(defn translate
  "Add a translation to matrix m"
  [m dx dy dz]
  (mat/mul (translation dx dy dz) m))
(s/fdef translate
  :args (s/cat :m ::mat/matrix
               :dx number?
               :dy number?
               :dz number?)
  :ret ::mat/matrix)

(defn scaling
  [dx dy dz]
  (mat/matrix [dx  0  0 0]
              [ 0 dy  0 0]
              [ 0  0 dz 0]
              [ 0  0  0 1]))
(s/fdef scaling
  :args (s/cat :dx number?
               :dy number?
               :dz number?)
  :ret ::mat/matrix)

(defn scale
  "Add a scaling to matrix m"
  [m dx dy dz]
  (mat/mul (scaling dx dy dz) m))
(s/fdef scale
  :args (s/cat :m ::mat/matrix
               :dx number?
               :dy number?
               :dz number?)
  :ret ::mat/matrix)

(defn rotation-x
  "Return a transformation to rotate around the x-axis by `rads` radians."
  [rads]
  (let [cos (Math/cos rads)
        sin (Math/sin rads)]
    (mat/matrix [1  0     0    0]
                [0 cos (- sin) 0]
                [0 sin   cos   0]
                [0  0     0    1])))
(s/fdef rotation-x
  :args (s/cat :rads number?)
  :ret ::mat/matrix)

(defn rotate-x
  "Add a rotation around the x-axis by `rads` radians to `m`."
  [m rads]
  (mat/mul (rotation-x rads) m))
(s/fdef rotate-x
  :args (s/cat :m ::mat/matrix
               :rads number?)
  :ret ::mat/matrix)

(defn rotation-y
  "Return a transformation to rotate around the y-axis by `rads` radians."
  [rads]
  (let [cos (Math/cos rads)
        sin (Math/sin rads)]
    (mat/matrix [  cos   0 sin 0]
                [   0    1  0  0]
                [(- sin) 0 cos 0]
                [   0    0  0  1])))
(s/fdef rotation-y
  :args (s/cat :rads number?)
  :ret ::mat/matrix)

(defn rotate-y
  "Add a rotation around the y-axis by `rads` radians to `m`."
  [m rads]
  (mat/mul (rotation-y rads) m))
(s/fdef rotate-y
  :args (s/cat :m ::mat/matrix
               :rads number?)
  :ret ::mat/matrix)

(defn rotation-z
  "Return a transformation to rotate around the z-axis by `rads` radians."
  [rads]
  (let [cos (Math/cos rads)
        sin (Math/sin rads)]
    (mat/matrix [cos (- sin) 0 0]
                [sin   cos   0 0]
                [ 0     0    1 0]
                [ 0     0    0 1])))
(s/fdef rotation-z
  :args (s/cat :rads number?)
  :ret ::mat/matrix)

(defn rotate-z
  "Add a rotation around the z-axis by `rads` radians to `m`."
  [m rads]
  (mat/mul (rotation-z rads) m))
(s/fdef rotate-z
  :args (s/cat :m ::mat/matrix
               :rads number?)
  :ret ::mat/matrix)

(defn- shearing*
  [xy xz yx yz zx zy]
  (mat/matrix [ 1 xy xz 0]
              [yx  1 yz 0]
              [zx zy  1 0]
              [ 0  0  0 1]))
(s/fdef shearing*
  :args (s/cat :xy number?
               :xz number?
               :yz number?
               :yz number?
               :zx number?
               :zy number?)
  :ret ::mat/matrix)

(defn shearing
  [& {:keys [xy xz
             yx yz
             zx zy]
      :or {xy 0 xz 0
           yx 0 yz 0
           zx 0 zy 0}}]
  (shearing* xy xz yx yz zx zy))

(s/def ::xy number?)
(s/def ::xz number?)
(s/def ::yz number?)
(s/def ::yz number?)
(s/def ::zx number?)
(s/def ::zy number?)
(s/fdef shearing
  :args (s/keys* :opt-un [::xy ::xz ::yx ::yz ::zx ::zy])
  :ret ::mat/matrix)

(defn shear
  [m & {:keys [xy xz
               yx yz
               zx zy]
        :or {xy 0 xz 0
             yx 0 yz 0
             zx 0 zy 0}}]
  (mat/mul (shearing* xy xz yx yz zx zy) m))
(s/fdef shear
  :args (s/cat :m ::mat/matrix
               :rest (s/keys* :opt-un [::xy ::xz ::yx ::yz ::zx ::zy]))
  :ret ::mat/matrix)

(defn identity
  "Return a 4x4 identity matrix"
  []
  (mat/identity 4))
(s/fdef identity
  :ret ::mat/matrix)

(defn apply
  "Apply a transform to a point or vector"
  [transform tuple]
  (mat/mult transform tuple))
(s/fdef apply
  :args (s/cat :transform ::mat/matrix
               :tuple ::tup/tuple)
  :ret ::tup/tuple)

(defn view-transform
  [from to up]
  (let [forward (tup/normalise (tup/sub to from))
        left (tup/cross forward (tup/normalise up))
        true-up (tup/cross left forward)
        orientation (mat/matrix (vec left)
                                (vec true-up)
                                (vec (tup/negate forward))
                                [0 0 0 1])]
    (mat/mul orientation
             (clojure.core/apply translation
                                 (take 3 (tup/negate from))))))
(s/fdef view-transform
  :args (s/cat :from ::tup/point
               :to ::tup/point
               :up ::tup/vector)
  :ret ::mat/matrix)
