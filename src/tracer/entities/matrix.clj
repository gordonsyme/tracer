(ns tracer.entities.matrix
  (:refer-clojure :exclude [get identity])
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.tuple :as t]))

(s/def ::row (s/and vector? (s/coll-of double?)))
(s/def ::matrix (s/and vector?
                       (s/coll-of ::row)))

(defn matrix
  [& rows]
  {:pre [(= 1 (->> (map count rows)
                   set
                   count))]}
   (vec (for [r rows]
          (mapv double r))))
(s/fdef matrix
  :args (s/cat :rows (s/* (s/coll-of number?)))
  :ret ::matrix)

(defn identity
  [n]
  (let [row (vec (repeat n 0.0))]
    (vec
      (for [i (range n)]
        (assoc row i 1.0)))))
(s/fdef identity
  :args (s/cat :n pos-int?)
  ;; add :fn spec to assert that rows/cols == n
  :ret ::matrix)

(defn get
  [m i j]
  (get-in m [i j]))
(s/fdef get
  :args (s/cat :m ::matrix
               :i nat-int?
               :j nat-int?)
  :ret double?)

(defn transpose
  [m]
  (vec
    (for [i (range (count (first m)))]
      (mapv #(clojure.core/get % i) m))))
(s/fdef transpose
  :args (s/cat :m ::matrix)
  ;; add fn spec to assert the count of rows/cols has swapped
  :ret ::matrix)

(defn mul
  "Multiply two 4x4 matrices."
  [a b]
  (let [[[a00 a01 a02 a03]
         [a10 a11 a12 a13]
         [a20 a21 a22 a23]
         [a30 a31 a32 a33]] a
        [[b00 b01 b02 b03]
         [b10 b11 b12 b13]
         [b20 b21 b22 b23]
         [b30 b31 b32 b33]] b]
  [[(+ (* a00 b00) (* a01 b10) (* a02 b20) (* a03 b30)) (+ (* a00 b01) (* a01 b11) (* a02 b21) (* a03 b31)) (+ (* a00 b02) (* a01 b12) (* a02 b22) (* a03 b32)) (+ (* a00 b03) (* a01 b13) (* a02 b23) (* a03 b33))]
   [(+ (* a10 b00) (* a11 b10) (* a12 b20) (* a13 b30)) (+ (* a10 b01) (* a11 b11) (* a12 b21) (* a13 b31)) (+ (* a10 b02) (* a11 b12) (* a12 b22) (* a13 b32)) (+ (* a10 b03) (* a11 b13) (* a12 b23) (* a13 b33))]
   [(+ (* a20 b00) (* a21 b10) (* a22 b20) (* a23 b30)) (+ (* a20 b01) (* a21 b11) (* a22 b21) (* a23 b31)) (+ (* a20 b02) (* a21 b12) (* a22 b22) (* a23 b32)) (+ (* a20 b03) (* a21 b13) (* a22 b23) (* a23 b33))]
   [(+ (* a30 b00) (* a31 b10) (* a32 b20) (* a33 b30)) (+ (* a30 b01) (* a31 b11) (* a32 b21) (* a33 b31)) (+ (* a30 b02) (* a31 b12) (* a32 b22) (* a33 b32)) (+ (* a30 b03) (* a31 b13) (* a32 b23) (* a33 b33))]]))
(s/fdef mul
  :args (s/cat :a ::matrix
               :b ::matrix)
  :ret ::matrix)

(defn mult
  "Multiply a matrix by a tuple.

  Returns a tuple."
  [mat tup]
  (let [[[a00 a01 a02 a03]
         [a10 a11 a12 a13]
         [a20 a21 a22 a23]
         [a30 a31 a32 a33]] mat
        [b00 b10 b20 b30] tup]
    [(+ (* a00 b00) (* a01 b10) (* a02 b20) (* a03 b30))
     (+ (* a10 b00) (* a11 b10) (* a12 b20) (* a13 b30))
     (+ (* a20 b00) (* a21 b10) (* a22 b20) (* a23 b30))
     (+ (* a30 b00) (* a31 b10) (* a32 b20) (* a33 b30))]))
(s/fdef mult
  :args (s/cat :mat ::matrix
               :tup ::t/tuple)
  :ret ::t/tuple)

(defn- scalar-div
  "Divide all elements of a matrix by a scalar."
  [m v]
  (vec
    (for [row m]
      (mapv #(/ % v) row))))
(s/fdef scalar-div
  :args (s/cat :m ::matrix
               :v (s/and number? (complement zero?)))
  :ret ::matrix)

;; eww :( might be possible to do via a ' or * variant of cofactor or determinant
(declare cofactor)
(defn determinant
  [m]
  (let [width (count (first m))
        height (count m)]
    (cond
      (= 1 width height)
      (get m 0 0)

      (= 2 width height)
      (- (* (get m 0 0)
            (get m 1 1))
         (* (get m 0 1)
            (get m 1 0)))

      :else
      (reduce +
              0
              (map-indexed
                (fn [j v]
                  (* v (cofactor m 0 j)))
                (first m))))))
(s/fdef determinant
  :args (s/cat :m ::matrix)
  :ret number?)

(defn- dissocv
  "Remove the element at `idx` from vector `v`"
  [v idx]
  (cond
    (zero? idx)
    (subvec v 1)

    (= idx (dec (count v)))
    (subvec v 0 idx)

    :else
    (into (subvec v 0 idx)
          (subvec v (inc idx)))))

(defn submatrix
  [m i j]
  (mapv #(dissocv % j)
        (dissocv m i)))
(s/fdef submatrix
  :args (s/cat :m ::matrix
               :i nat-int?
               :j nat-int?)
  :ret ::matrix)

(def minor
  "The minor of a matrix at [i, j] is defined to be the determinant of the
  submatrix at [i, j]"
  (comp determinant submatrix))
(s/fdef minor
  :args (s/cat :m ::matrix
               :i nat-int?
               :j nat-int?)
  :ret number?)

(defn cofactor
  [m i j]
  (if (odd? (+ i j))
    (- (minor m i j))
    (minor m i j)))
(s/fdef minor
  :args (s/cat :m ::matrix
               :i nat-int?
               :j nat-int?)
  :ret number?)

(def invertible? (comp (complement zero?) determinant))
(s/fdef invertible?
  :args (s/cat :m ::matrix)
  :ret boolean?)

(defn- cofactor-matrix
  [m]
  (vec
    (map-indexed
      (fn [i row]
        (vec
          (map-indexed
            (fn [j _col]
              (cofactor m i j))
            row)))
      m)))

(defn inverse
  [m]
  (-> (cofactor-matrix m)
      (transpose)
      (scalar-div (determinant m))))
(s/fdef inverse
  :args (s/cat :m ::matrix)
  :ret ::matrix)
