(ns tracer.types
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

(def ^:const ^:private double-array-type (class (double-array 0)))

(defmethod print-method double-array-type
  [x ^java.io.Writer w]
  (.write w "[D")
  (print-method (vec x) w))

(defn double-array?
  [x]
  (instance? double-array-type x))
(s/fdef double-array?
  :args (s/cat :x any?)
  :ret boolean?)

(s/def ::double-array (s/with-gen
                        double-array?
                        #(gen/fmap double-array (s/gen (s/coll-of double?)))))
