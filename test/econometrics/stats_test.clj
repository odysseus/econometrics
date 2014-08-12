(ns econometrics.stats-test
  (:require [clojure.test :refer :all]
            [econometrics.constants :refer :all]
            [econometrics.integrals :refer :all]
            [econometrics.curves :refer :all]
            [econometrics.stats :refer :all]))

(deftest test-square
  (let [t (square 4)]
    (is (= t 16) "4 squared should be 16")))

(deftest test-zip
  (let [a '(1 2 3)
        b '(4 5 6)
        c '(7 8 9)
        z (zip a b c)]
    (is (= (first z) '(1 4 7))
        "The first item of 3 interleaved lists should be a list containing
        the first item of each list")))

(deftest test-rand-in-range
  (let [n (rand-in-range 10 15)]
    (is (>= n 10))
    (is (<= n 15))))

(deftest test-random-sequence
  (let [s (random-sequence 100 10 20)]
    (is (= 100 (count s)))
    (is (every? true? (map #(and (>= % 10) (<= % 20)) s)))))

(deftest test-mean
  (let [r (range 1 11)]
    (is (= 5.5 (mean r)))))

(deftest test-median
  (let [odd '(5 4 2 3 1 8 6 9 7)
        even (range 1 9)]
    (is (= 5.0 (median odd)))
    (is (= 4.5 (median even)))))

(deftest test-range
  (let [s (range 10 21)]
    (is (= 10.0 (range-stat s)))))

(deftest test-partition-by-values
  (let [s '(3 1 3 2 4 1 2 5 1 1)
        part (partition-by-values s)]
    (is (= (first part) '(1 1 1 1)))
    (is (= (second part) '(2 2)))))

(deftest test-mode
  (let [s '(1 3 4 5 1 4 2 0 9 8 1 1 9 5 1 1 1)]
    (is (= 1 (mode s)))))

(let [a (range 1 10)]

  (deftest test-error-terms
    (is (= (error-terms a) '(-4.0 -3.0 -2.0 -1.0 0.0 1.0 2.0 3.0 4.0))))

  (deftest test-squared-errors
    (is (= (squared-errors a) '(16.0 9.0 4.0 1.0 0.0 1.0 4.0 9.0 16.0))))

  (deftest test-sum-squared-errors
    (is (= (sum-squared-errors a) 60.0)))

  (deftest test-population-variance
    (is (= 6.667 (round (population-variance a) 3))))

  (deftest test-sample-variance
    (is (= 7.5 (sample-variance a))))

  (deftest test-population-sigma
    (is (= 2.582 (round (population-sigma a) 3))))

  (deftest test-sample-sigma
    (is (= 2.739 (round (sample-sigma a) 3))))

  (deftest test-population-z-scores
    (let [m (population-z-scores-map a)]
      (is (= -0.775 (round (m 3) 3)))))

  (deftest test-sample-z-scores
    (let [m (sample-z-scores-map a)]
      (is (= -0.730 (round (m 3) 3))))))
