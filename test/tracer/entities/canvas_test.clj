(ns tracer.entities.canvas-test
  (:require [clojure.test :refer (deftest testing is)]
            [orchestra.spec.test :as s-test]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.colour :as colour]
            [tracer.entities.canvas :as sut]))

(clojure.test/use-fixtures :once instrument)

(deftest creating-a-canvas
  (let [c (sut/canvas 10 20)]
    (is (= 10 (sut/width c)))
    (is (= 20 (sut/height c)))
    (is #{(colour/colour 0 0 0)}
        (set (seq (sut/pixels c))))))

(deftest writing-pixels-to-a-canvas
  (let [c (sut/canvas 10 20)
        red (colour/colour 1 0 0)]
    (let [new-c (sut/write-pixel c 2 3 red)]
      (is (= red (sut/pixel-at new-c 2 3))))

    (testing "writing out of bounds"
      ;; can't use invalid bounds otherwise
      (s-test/unstrument `sut/write-pixel)

      (is (= c (sut/write-pixel c -1 0 red)))
      (is (= c (sut/write-pixel c 10 0 red)))
      (is (= c (sut/write-pixel c 0 -1 red)))
      (is (= c (sut/write-pixel c 0 20 red))))))

(deftest row-gets-rows
  (let [red (colour/colour 1 0 0)
        green (colour/colour 0 1 0)
        blue (colour/colour 0 0 1)
        c (assoc
            (sut/canvas 5 3)
            ::sut/pixels (vec (concat
                                (repeat 5 red)
                                (repeat 5 green)
                                (repeat 5 blue))))]
    (is (= (vec (repeat 5 red))
           (sut/row c 0)))
    (is (= (vec (repeat 5 green))
           (sut/row c 1)))
    (is (= (vec (repeat 5 blue))
           (sut/row c 2)))))
