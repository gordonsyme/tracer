(ns tracer.entities.camera
  (:require [clojure.spec.alpha :as s]
            [tracer.entities.canvas :as canvas]
            [tracer.entities.matrix :as mat]
            [tracer.entities.ray :as ray]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]
            [tracer.entities.world :as world]))

(s/def ::hsize pos-int?)
(s/def ::half-width (s/and number? pos?))
(s/def ::vsize pos-int?)
(s/def ::half-height (s/and number? pos?))
(s/def ::field-of-view (s/and number? pos?))
(s/def ::pixel-size number?)
(s/def ::transform ::mat/matrix)
(s/def ::inverse-transform ::mat/matrix)
(s/def ::camera (s/keys :req-un [::hsize
                                 ::vsize
                                 ::half-width
                                 ::half-height
                                 ::field-of-view
                                 ::pixel-size
                                 ::transform
                                 ::inverse-transform]))

(defn camera
  [hsize vsize field-of-view]
  (let [half-view (Math/tan (/ field-of-view 2))
        aspect (/ hsize vsize)
        [half-width half-height] (if (>= aspect 1)
                                   [half-view (/ half-view aspect)]
                                   [(* half-view aspect) half-view])]
    {:hsize hsize
     :vsize vsize
     :half-width half-width
     :half-height half-height
     :field-of-view field-of-view
     :pixel-size (/ (* half-width 2) hsize)
     :transform (transform/identity)
     :inverse-transform (transform/identity)}))
(s/fdef camera
  :args (s/cat :hsize ::hsize
               :vsize ::vsize
               :field-of-view ::field-of-view)
  :ret ::camera)

(defn with-transform
  [c m]
  (assoc c :transform m
           :inverse-transform (mat/inverse m)))
(s/fdef with-transform
  :args (s/cat :c ::camera
               :m ::mat/matrix)
  :ret ::camera)

(defn ray-for-pixel
  [c px py]
  (let [x-offset (* (+ px 0.5)
                    (:pixel-size c))
        y-offset (* (+ py 0.5)
                    (:pixel-size c))
        world-x (- (:half-width c) x-offset)
        world-y (- (:half-height c) y-offset)
        pixel (mat/mult (:inverse-transform c)
                        (tup/point world-x world-y -1))
        origin (mat/mult (:inverse-transform c)
                         (tup/point 0 0 0))]
    (ray/ray origin (tup/normalise (tup/sub pixel origin)))))
(s/fdef ray-for-pixel
  :args (s/cat :c ::camera
               :px (s/and int? (complement neg?))
               :py (s/and int? (complement neg?)))
  :ret ::ray/ray)

(defn render
  [c w]
  (let [width (:hsize c)
        height (:vsize c)
        pixels-per-thread (Math/ceil
                            (/ (* width height)
                               (-> (Runtime/getRuntime)
                                   (.availableProcessors)
                                   (+ 2))))
        ranges (partition-all
                 pixels-per-thread
                 (for [x (range width)
                       y (range height)]
                   [x y]))
        workers (->> (for [r ranges]
                       (future
                         (mapv (fn [[x y]]
                                 (let [r (ray-for-pixel c x y)]
                                   [x y (world/colour-at w r)]))
                               r)))
                     doall
                     (map deref)
                     doall)
        pixels (apply concat workers)]
    (reduce (fn [canvas [x y colour]]
              (canvas/write-pixel canvas x y colour))
            (canvas/canvas width height)
            (filter some? pixels))))
(s/fdef render
  :args (s/cat :c ::camera
               :w ::world/world)
  :ret ::canvas/canvas)
