(ns tracer.entities.pattern-test
  (:require [clojure.test :refer (deftest testing is)]
            [clojure.spec.alpha :as s]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.colour :as colour]
            [tracer.entities.matrix :as mat]
            [tracer.entities.pattern :as pattern]
            [tracer.entities.shape :as shape]
            [tracer.entities.sphere :as sphere]
            [tracer.entities.transform :as transform]
            [tracer.entities.tuple :as tup]))

(clojure.test/use-fixtures :once instrument)

(def ^:private black (colour/colour 0 0 0 ))
(def ^:private black-pattern (pattern/colour-pattern black))
(def ^:private white (colour/colour 1 1 1 ))
(def ^:private white-pattern (pattern/colour-pattern white))

(defn- test-pattern
  []
  {:transform (transform/identity)
   :inverse-transform (transform/identity)
   :shader (fn [point]
             (colour/colour (tup/x point) (tup/y point) (tup/z point)))})
(s/fdef test-pattern
  :ret ::pattern/pattern)

(deftest patterns-can-have-transformation
  (let [p (pattern/with-transform (test-pattern)
            (transform/translation 1 2 3))]
    (is (= (transform/translation 1 2 3)
           (:transform p)))
    (is (= (mat/inverse (transform/translation 1 2 3))
           (:inverse-transform p)))))

(deftest striped-patterns
  (let [p (pattern/stripe-pattern white-pattern black-pattern)]
    (testing "a stripe pattern is constant in y"
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0))))
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0))))
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0)))))

    (testing "a stripe pattern is constant in z"
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0))))
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 1))))
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 2)))))

    (testing "a stripe pattern alternates in x")
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0))))
      (is (= white (#'pattern/local-colour-at p (tup/point 0.9 0 0))))
      (is (= black (#'pattern/local-colour-at p (tup/point 1 0 0))))
      (is (= black (#'pattern/local-colour-at p (tup/point -0.1 0 0))))
      (is (= black (#'pattern/local-colour-at p (tup/point -1 0 0))))
      (is (= white (#'pattern/local-colour-at p (tup/point -1.1 0 0))))))

(deftest patterns-with-transformations
  (testing "pattern with an object transformation"
    (let [object (shape/with-transform (sphere/sphere)
                   (transform/scaling 2 2 2))
          p (test-pattern)]
      (is (= (colour/colour 1 1.5 2)
             (pattern/colour-at p (shape/inverse-transform object) (tup/point 2 3 4))))))

  (testing "stripes with a pattern transformation"
    (let [object (sphere/sphere)
          p (pattern/with-transform (test-pattern)
              (transform/scaling 2 2 2))]
      (is (= (colour/colour 1 1.5 2)
             (pattern/colour-at p (shape/inverse-transform object) (tup/point 2 3 4))))))

  (testing "stripes with a pattern and object transformation"
    (let [object (shape/with-transform (sphere/sphere)
                   (transform/scaling 2 2 2))
          p (pattern/with-transform (test-pattern)
              (transform/translation 0.5 1 1.5))]
      (is (= (colour/colour 0.75 0.5 0.25)
             (pattern/colour-at p (shape/inverse-transform object) (tup/point 2.5 3 3.5)))))))

(deftest gradient-pattern-linearly-interpolates-between-colours
  (let [p (pattern/gradient white black)]
    (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0))))
    (is (= (colour/colour 0.75 0.75 0.75)
           (#'pattern/local-colour-at p (tup/point 0.25 0 0))))
    (is (= (colour/colour 0.5 0.5 0.5)
           (#'pattern/local-colour-at p (tup/point 0.5 0 0))))
    (is (= (colour/colour 0.25 0.25 0.25)
           (#'pattern/local-colour-at p (tup/point 0.75 0 0))))))

(deftest radial-pattern
  (let [p (pattern/rings white black)]
    (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0))))
    (is (= black (#'pattern/local-colour-at p (tup/point 1 0 0))))
    (is (= black (#'pattern/local-colour-at p (tup/point 0 0 1))))
    (is (= black (#'pattern/local-colour-at p (tup/point 0.708 0 0.708))))))

(deftest three-d-checked-pattern
  (testing "checks should repeat in x"
    (let [p (pattern/checks white black)]
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0))))
      (is (= white (#'pattern/local-colour-at p (tup/point 0.99 0 0))))
      (is (= black (#'pattern/local-colour-at p (tup/point 1.01 0 0))))))

  (testing "checks should repeat in y"
    (let [p (pattern/checks white black)]
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0))))
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0.99 0))))
      (is (= black (#'pattern/local-colour-at p (tup/point 0 1.01 00))))))

  (testing "checks should repeat in z"
    (let [p (pattern/checks white black)]
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0))))
      (is (= white (#'pattern/local-colour-at p (tup/point 0 0 0.99))))
      (is (= black (#'pattern/local-colour-at p (tup/point 0 0 1.01)))))))

(deftest blending-patterns
  (let [p (pattern/blended-pattern white-pattern black-pattern)
        grey (colour/colour 0.5 0.5 0.5)]
      (is (= grey (#'pattern/local-colour-at p (tup/point 0 0 0))))
      (is (= grey (#'pattern/local-colour-at p (tup/point 0 0 0.99))))
      (is (= grey (#'pattern/local-colour-at p (tup/point 0 0 1.01))))))
