(ns tracer.entities.matrix-test
  (:require [clojure.test :refer (deftest testing is)]
            [tracer.comparators :refer (approx)]
            [tracer.fixtures :refer (instrument)]
            [tracer.entities.tuple :as t]
            [tracer.entities.matrix :as mat]))

(clojure.test/use-fixtures :once instrument)

(deftest rows-must-be-the-same-size
  (is (thrown? AssertionError
        (mat/matrix [ 1    2    3    4]
                    [ 5.5  6.5  7.5]))))

(deftest constructing-and-creating-a-4x4-matrix
  (let [m (mat/matrix [ 1    2    3    4]
                      [ 5.5  6.5  7.5  8.5]
                      [ 9   10   11   12]
                      [13.5 14.5 15.5 16.5])]
    (is (== 1 (mat/get m 0 0)))
    (is (== 4 (mat/get m 0 3)))
    (is (== 5.5 (mat/get m 1 0)))
    (is (== 7.5 (mat/get m 1 2)))
    (is (== 11 (mat/get m 2 2)))
    (is (== 13.5 (mat/get m 3 0)))
    (is (== 15.5 (mat/get m 3 2)))))

(deftest a-2x2-matrix-can-be-represented
  (let [m (mat/matrix [-3  5]
                      [ 1 -2])]
    (is (== -3 (mat/get m 0 0)))
    (is (== 5 (mat/get m 0 1)))
    (is (== 1 (mat/get m 1 0)))
    (is (== -2 (mat/get m 1 1)))))

(deftest a-3x3-matrix-can-be-represented
  (let [m (mat/matrix [-3  5  0]
                      [ 1 -2 -7]
                      [ 0  1  1])]
    (is (== -3 (mat/get m 0 0)))
    (is (== -2 (mat/get m 1 1)))
    (is (== 1 (mat/get m 2 2)))))

(deftest equality-with-identical-matrices
  (let [a (mat/matrix [1 2 3 4]
                      [5 6 7 8]
                      [9 8 7 6]
                      [5 4 3 2])
        b (mat/matrix [1 2 3 4]
                      [5 6 7 8]
                      [9 8 7 6]
                      [5 4 3 2])]
    (is (= a b))
    (is (= a a))
    (is (approx a b))
    (is (approx a a))))

(deftest equality-with-different-matrices
  (let [a (mat/matrix [1 2 3 4]
                      [5 6 7 8]
                      [9 8 7 6]
                      [5 4 3 2])
        b (mat/matrix [2 3 4 5]
                      [6 7 8 9]
                      [8 7 6 5]
                      [4 3 2 1])]
    (is (not= a b))
    (is (not (approx a b)))))

(deftest multiplying-two-matrices
  (let [a (mat/matrix [1 2 3 4]
                      [5 6 7 8]
                      [9 8 7 6]
                      [5 4 3 2])
        b (mat/matrix [-2 1 2  3]
                      [ 3 2 1 -1]
                      [ 4 3 6  5]
                      [ 1 2 7  8])]
    (is (= (mat/matrix
             [20 22  50  48]
             [44 54 114 108]
             [40 58 110 102]
             [16 26  46  42])
           (mat/mul a b)))))

(deftest a-matrix-multiplied-by-a-tuple
  (let [a (mat/matrix [1 2 3 4]
                      [2 4 4 2]
                      [8 6 4 1]
                      [0 0 0 1])
        b (t/tuple 1 2 3 1)]
    (is (= (t/tuple 18 24 33 1)
           (mat/mult a b)))))

(deftest identity-makes-identity-matrices
  (is (= (mat/matrix [1 0]
                     [0 1])
         (mat/identity 2)))
  (is (= (mat/matrix [1 0 0]
                     [0 1 0]
                     [0 0 1])
         (mat/identity 3)))
  (is (= (mat/matrix [1 0 0 0]
                     [0 1 0 0]
                     [0 0 1 0]
                     [0 0 0 1])
         (mat/identity 4))))

(deftest multiplying-a-matrix-by-the-identity-matrix
  (let [a (mat/matrix [0 1  2  4]
                      [1 2  4  8]
                      [2 4  8 16]
                      [4 8 16 32])]
    (is (= a
           (mat/mul (mat/identity 4) a)))
    (is (= a
           (mat/mul a (mat/identity 4))))))

(deftest multiplying-the-identity-matrix-by-a-tuple
  (let [a (t/tuple 1 2 3 4)]
    (is (= a (mat/mult (mat/identity 4) a)))))

(deftest transposing-a-matrix
  (let [a (mat/matrix [0 9 3 0]
                      [9 8 0 8]
                      [1 8 5 3]
                      [0 0 5 8])]
    (is (= (mat/matrix [0 9 1 0]
                       [9 8 8 0]
                       [3 0 5 5]
                       [0 8 3 8])
           (mat/transpose a))))

  (testing "transposing the identity matrix is a noop"
    (is (= (mat/identity 4)
           (mat/transpose (mat/identity 4))))))

(deftest calculating-the-determinant-of-a-2x2-matrix
  (is (== 17 (mat/determinant (mat/matrix [ 1 5]
                                          [-3 2])))))

(deftest submatrices
  (testing "a submatrix of a 3x3 matrix is a 2x2 matrix"
    (let [a (mat/matrix [ 1 5  0]
                        [-3 2  7]
                        [ 0 6 -3])]
      (is (= (mat/matrix [2  7]
                         [6 -3])
             (mat/submatrix a 0 0)))
      (is (= (mat/matrix [-3  7]
                         [ 0 -3])
             (mat/submatrix a 0 1)))
      (is (= (mat/matrix [-3 2]
                         [ 0 6])
             (mat/submatrix a 0 2)))

      (is (= (mat/matrix [5  0]
                         [6 -3])
             (mat/submatrix a 1 0)))
      (is (= (mat/matrix [1  0]
                         [0 -3])
             (mat/submatrix a 1 1)))
      (is (= (mat/matrix [1 5]
                         [0 6])
             (mat/submatrix a 1 2)))

      (is (= (mat/matrix [5 0]
                         [2 7])
             (mat/submatrix a 2 0)))
      (is (= (mat/matrix [ 1 0]
                         [-3 7])
             (mat/submatrix a 2 1)))
      (is (= (mat/matrix [ 1 5]
                         [-3 2])
             (mat/submatrix a 2 2)))))

  (testing "a submatrix of a 4x4 matrix is a 3x3 matrix"
    (let [a (mat/matrix [-6 1  1 6]
                        [-8 5  8 6]
                        [-1 0  8 2]
                        [-7 1 -1 1])]
      (is (= (mat/matrix [-6  1 6]
                         [-8  8 6]
                         [-7 -1 1])
             (mat/submatrix a 2 1))))))

(deftest calculating-a-minor-of-a-3x3-matrix
  (let [a (mat/matrix [3  5  0]
                      [2 -1 -7]
                      [6 -1  5])
        b (mat/submatrix a 1 0)]
    (is (== 25 (mat/determinant b)))
    (is (== 25 (mat/minor a 1 0)))))

(deftest calculating-a-cofactor-of-a-3x3-matrix
  (let [a (mat/matrix [3  5  0]
                      [2 -1 -7]
                      [6 -1  5])]
    (is (== -12 (mat/minor a 0 0)))
    (is (== -12 (mat/cofactor a 0 0)))
    (is (== 25 (mat/minor a 1 0)))
    (is (== -25 (mat/cofactor a 1 0)))))

(deftest calculating-determinants
  (testing "3x3 matrix"
    (let [a (mat/matrix [ 1 2  6]
                        [-5 8 -4]
                        [ 2 6  4])]
      (is (== 56 (mat/cofactor a 0 0)))
      (is (== 12 (mat/cofactor a 0 1)))
      (is (== -46 (mat/cofactor a 0 2)))
      (is (== -196 (mat/determinant a)))))

  (testing "4x4 matrix"
    (let [a (mat/matrix [-2 -8  3  5]
                        [-3  1  7  3]
                        [ 1  2 -9  6]
                        [-6  7  7 -9])]
      (is (== 690 (mat/cofactor a 0 0)))
      (is (== 447 (mat/cofactor a 0 1)))
      (is (== 210 (mat/cofactor a 0 2)))
      (is (== 51 (mat/cofactor a 0 3)))
      (is (== -4071 (mat/determinant a))))))
