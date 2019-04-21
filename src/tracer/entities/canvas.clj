(ns tracer.entities.canvas
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.colour :as colour]))

(s/def ::width nat-int?)
(s/def ::height nat-int?)
(s/def ::pixels (s/coll-of ::colour/colour))
(s/def ::canvas (s/keys :req [::width ::height ::pixels]))

(defn canvas
  [width height]
  {::width width
   ::height height
   ::pixels (vec (repeat (* width height)
                         (colour/colour 0 0 0)))})
(s/fdef canvas
  :args (s/cat :width ::width
               :height ::height)
  :ret ::canvas)

(defn width
  [c]
  (::width c))
(s/fdef width
  :args (s/cat :c ::canvas)
  :ret ::width)

(defn height
  [c]
  (::height c))
(s/fdef height
  :args (s/cat :c ::canvas)
  :ret ::height)

(defn pixels
  [c]
  (::pixels c))
(s/fdef pixels
  :args (s/cat :c ::canvas)
  :ret ::pixels)

(defn write-pixel
  [c x y colour]
  (let [pos (+ x (* y (::width c)))]
    (update c ::pixels assoc pos colour)))
(s/fdef write-pixel
  :args (s/cat :c ::canvas
               :x nat-int?
               :y nat-int?
               :colour ::colour/colour)
  :ret ::canvas)

(defn pixel-at
  [c x y]
  (let [pos (+ x (* y (::width c)))]
    (nth (::pixels c) pos)))
(s/fdef pixel-at
  :args (s/cat :c ::canvas
               :x nat-int?
               :y nat-int?)
  :ret ::colour/colour)

(defn row
  [c r]
  (subvec (::pixels c)
          (* r (::width c))
          (* (inc r) (::width c))))
(s/fdef row
  :args (s/cat :c ::canvas
               :r nat-int?)
  :ret ::pixels)
