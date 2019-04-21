(ns tracer.use-cases.ppm
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [tracer.entities.colour :as colour]
            [tracer.entities.canvas :as canvas]))

(defn- colour->ppm
  [c]
  (letfn [(convert [x]
            (min 255
                 (max 0
                      (int (Math/ceil (* 255 x))))))]
    [(convert (colour/red c))
     (convert (colour/green c))
     (convert (colour/blue c))]))

(defn- stringify-row
  [r]
  (loop [acc []
         s (string/join " " (flatten (map colour->ppm r)))]
    (cond
      (<= (count s) 70)
      (conj acc (string/trimr s))

      :else
      (let [split-index (inc
                          (loop [i 69]
                            (if (= \space (nth s i))
                              i
                              (recur (dec i)))))]
        (recur (conj acc
                     (string/trimr (subs s 0 split-index)))
               (subs s split-index))))))
(s/fdef stringify-row
  :args (s/cat :r (s/coll-of ::colour/colour))
  :ret (s/coll-of string?))

(defn- rows->ppm
  [c row]
  (if (>= row (canvas/height c))
    []
    (let [r (canvas/row c row)]
      (lazy-cat (stringify-row r)
                (rows->ppm c (inc row))))))
(s/fdef rows->ppm
  :args (s/cat :c ::canvas/canvas
               :row nat-int?)
  ;; TODO :fn spec pls
  :ret (s/coll-of string?))

(defn- gen-lines
  [c]
  (lazy-cat ["P3"
             (format "%d %d"
                     (canvas/width c)
                     (canvas/height c))
             "255"]
            (rows->ppm c 0)
            [""]))
(s/fdef gen-lines
  :args (s/cat :c ::canvas/canvas)
  :ret (s/coll-of string?))

(defn generate
  [c stream]
  (with-open [w (io/writer stream)]
    (doseq [l (gen-lines c)]
      (.write w l)
      (.newLine w))))
