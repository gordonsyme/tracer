(ns tracer.entities.camera-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.test-entities :refer (default-world)]
            [tracer.entities.camera :as camera]
            [tracer.entities.canvas :as canvas]
            [tracer.entities.colour :as colour]
            [tracer.entities.ray :as ray]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(deftest constructing-a-camera
  (let [hsize 160
        vsize 120
        fov (/ (Math/PI) 2)]
    (is (= {:hsize 160
            :vsize 120
            :field-of-view (/ Math/PI 2)
            :transform (transform/identity)}
           (select-keys
             (camera/camera hsize vsize fov)
             [:hsize :vsize :field-of-view :transform])))))

(deftest pixel-sizes
  (testing "horizontal canvas"
    (let [c (camera/camera 200 125 (/ Math/PI 2))]
      (is (approx 0.01 (:pixel-size c)))))
  (testing "vertical canvas"
    (let [c (camera/camera 125 200 (/ Math/PI 2))]
      (is (approx 0.01 (:pixel-size c))))))

(defn- approx-ray
  [r1 r2]
  (and (approx (:origin r1) (:origin r2))
       (approx (:direction r1) (:direction r2))))

(deftest rays-from-the-camera
  (let [c (camera/camera 201 101 (/ Math/PI 2))]
    (testing "constructing a ray through the centre of the canvas"
      (let [r (camera/ray-for-pixel c 100 50)]
        (is (approx-ray
              (ray/ray (tup/point 0 0 0)
                       (tup/vector 0 0 -1))
              r))))
    (testing "constructing a ray through a corner of the canvas"
      (let [r (camera/ray-for-pixel c 0 0)]
        (is (approx-ray
              (ray/ray (tup/point 0 0 0)
                       (tup/vector 0.66519 0.33259 -0.66851))
              r))))
    (testing "constructing a ray when the camera is transformed"
      (let [c (camera/with-transform c (-> (transform/identity)
                                           (transform/translate 0 -2 5)
                                           (transform/rotate-y (/ Math/PI 4))))
            r (camera/ray-for-pixel c 100 50)]
        (is (approx-ray
              (ray/ray (tup/point 0 2 -5)
                       (tup/vector (/ (Math/sqrt 2) 2)
                                   0
                                   (- (/ (Math/sqrt 2) 2))))
              r))))))

(deftest rendering-a-world-with-a-camera
  (let [w (default-world)
        c (camera/with-transform
            (camera/camera 11 11 (/ Math/PI 2))
            (transform/view-transform (tup/point 0 0 -5)
                                      (tup/point 0 0 0)
                                      (tup/vector 0 1 0)))
        image (camera/render c w)]
    (is (approx (colour/colour 0.38066 0.47583 0.2855)
                (canvas/pixel-at image 5 5)))))
