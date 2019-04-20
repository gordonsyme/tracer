(ns putting-together.chapter-1
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.tuple :as t]))

(s/def ::position ::t/point)
(s/def ::velocity ::t/vector)
(s/def ::projectile (s/keys :req-un [::position ::velocity]))

(defn projectile
  [p v]
  {:position p
   :velocity v})

(s/fdef projectile
  :args (s/cat :p ::t/point
               :v ::t/vector)
  :ret ::projectile)

(s/def ::gravity ::t/vector)
(s/def ::wind ::t/vector)
(s/def ::environment (s/keys :req-un [::gravity ::wind]))

(defn environment
  [gravity wind]
  {:gravity gravity
   :wind wind})

(s/fdef environment
  :args (s/cat :gravity ::t/vector
               :wind ::t/vector)
  :ret ::environment)

(defn tick
  [[env proj]]
  [env
   {:position (t/add (:position proj) (:velocity proj))
    :velocity (t/add (t/add (:velocity proj)
                            (:gravity env))
                     (:wind env))}])

(s/fdef tick
  :args (s/cat :env ::environment
               :proj ::projectile)
  :ret (s/cat :env ::environment
              :proj ::projectile))

(defn go
  []
  (let [e (environment (t/vector 0 -0.1 0) (t/vector -0.01 0 0))
        p (projectile (t/point 0 1 0) (t/normalise (t/vector 1 1 0)))
        position #(-> % second :position)]
    (->> (iterate tick [e p])
         (take-while #(-> % position second (>= 0)))
         (map (comp println position)))))
