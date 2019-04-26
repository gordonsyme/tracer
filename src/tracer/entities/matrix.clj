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
  [a b]
  (let [b (transpose b)
        dot (fn [a b]
              (reduce + (map * a b)))]
    (vec
      (for [row a]
        (mapv dot (repeat row) b)))))
(s/fdef mul
  :args (s/cat :a ::matrix
               :b ::matrix)
  :ret ::matrix)

(defn- tuple->matrix
  [tup]
  (mapv vector tup))
(s/fdef tuple->matrix
  :args (s/cat :tup ::t/tuple)
  :ret ::matrix)

(defn- matrix->tuple
  [mat]
  (vec (flatten mat)))
(s/fdef matrix->tuple
  :args (s/cat :mat ::matrix)
  :ret ::t/tuple)

(defn mult
  "Multiply a matrix by a tuple.

  Returns a tuple."
  [mat tup]
  (->> (tuple->matrix tup)
       (mul mat)
       (matrix->tuple)))
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
