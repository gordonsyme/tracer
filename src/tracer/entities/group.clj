(ns tracer.entities.group
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.intersection :as i]
            [tracer.entities.material :as material]
            [tracer.entities.shape :as shape]
            [tracer.entities.transform :as transform]))

(s/def ::group ::shape/common)

(defmethod shape/object-type :group
  [_]
  ::group)

(defn group
  []
  {::shape/tag :group
   :transform (transform/identity)
   :inverse-transform (transform/identity)
   :material (material/material)})
(s/fdef group
  :ret ::group)

(defn add-child
  [relations group shape]
  (-> relations
      (update-in [group :children] conj shape)
      (update-in [group :parent] identity)
      (assoc-in [shape :parent] group)))
(s/fdef add-child
  :args (s/cat :relations ::shape/relations
               :group ::group
               :shape ::shape/object)
  :ret ::shape/relations)

(defn children
  [relations group]
  (if-let [cs (get-in relations [group :children])]
    cs
    []))
(s/fdef children
  :args (s/cat :relations ::shape/relations
               :group ::group)
  :ret ::shape/children)

(defn parent
  [relations shape]
  (get-in relations [shape :parent]))
(s/fdef parent
  :args (s/cat :relations ::shape/relations
               :shape ::shape/object)
  :ret (s/nilable ::shape/object))

(defmethod shape/local-intersect :group
  [rels g ray]
  (->> (children rels g)
       (mapcat #(i/intersect rels % ray))
       (sort-by :t)))
