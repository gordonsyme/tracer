(ns tracer.entities.sphere
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.intersection :as i]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as ray]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as t]))

(s/def ::transform ::mat/matrix)
(s/def ::sphere (s/keys :req [::i/tag]
                        :req-un [::transform]))

(defmethod i/object-type :sphere
  [_]
  ::sphere)

(defn sphere
  []
  {::i/tag :sphere
   :transform (transform/identity)})
(s/fdef sphere
  :ret ::sphere)

(defmethod i/intersect :sphere
  [s ray]
  (let [r (ray/transform ray (mat/inverse (:transform s)))
        sphere-to-ray (t/sub (:origin r) (t/point 0 0 0))
        a (t/dot (:direction r) (:direction r))
        b (* 2 (t/dot (:direction r)
                      sphere-to-ray))
        c (dec (t/dot sphere-to-ray sphere-to-ray))
        discriminant (- (* b b)
                        (* 4 a c))]
    (if (neg? discriminant)
      []
      (let [root (Math/sqrt discriminant)
            ts [(/ (- (- b) root)
                   (* 2 a))
                (/ (+ (- b) root)
                   (* 2 a))]]
        (map (partial hash-map :object s :t) ts)))))

(defn with-transform
  [s m]
  (assoc s :transform m))
(s/fdef with-transform
  :args (s/cat :s ::sphere
               :m ::mat/matrix)
  :ret ::sphere)
