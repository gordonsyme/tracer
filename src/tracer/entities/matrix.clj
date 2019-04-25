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

(defn determinant
  [m]
  (- (* (get m 0 0)
        (get m 1 1))
     (* (get m 0 1)
        (get m 1 0))))
(s/fdef determinant
  :args (s/cat :m (s/and ::matrix
                         #(= 2 (count (first %)))))
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
