(ns tracer.use-cases.ppm-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.canvas :as canvas]
            [tracer.entities.colour :as colour]
            [tracer.use-cases.ppm :as ppm]))

(clojure.test/use-fixtures :once instrument)

(deftest constructing-the-ppm
  (testing "constructing the header"
    (let [c (canvas/canvas 5 3)
          p (#'ppm/gen-lines c)]
      (is (= ["P3"
              "5 3"
              "255"]
             (take 3 p)))))
  (testing "constructing the PPM pixel data"
    (let [c1 (colour/colour 1.5 0 0)
          c2 (colour/colour 0 0.5 0)
          c3 (colour/colour -0.5 0 1)
          p (-> (canvas/canvas 5 3)
                (canvas/write-pixel 0 0 c1)
                (canvas/write-pixel 2 1 c2)
                (canvas/write-pixel 4 2 c3)
                (#'ppm/gen-lines))]
      (is (= ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
             (take 3 (drop 3 p)))))))

(deftest splitting-long-lines-in-the-ppm
  (let [c (assoc
            (canvas/canvas 10 2)
            ::canvas/pixels (vec (repeat (* 10 2) (colour/colour 1 0.8 0.6))))
        p (#'ppm/gen-lines c)]
    (is (= ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
            "153 255 204 153 255 204 153 255 204 153 255 204 153"
            "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
            "153 255 204 153 255 204 153 255 204 153 255 204 153"]
           (take 4 (drop 3 p))))))

(deftest ppm-files-are-terminated-by-a-newline-character
  (let [c (canvas/canvas 5 3)
        p (#'ppm/gen-lines c)]
    (is (= ""
           (last p)))))

(deftest can-generate-ppm-quickly
  (let [width 1024
        height 768
        c (assoc
            (canvas/canvas width height)
            ::canvas/pixels (vec (repeat (* width height) (colour/colour 1 0.8 0.6))))]
    (with-open [s (clojure.java.io/output-stream (java.io.File. "/dev/null"))]
      (ppm/generate c s))))
