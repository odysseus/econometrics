(ns econometrics.stats
  (:require [clojure.core.reducers :as r]
            [econometrics.constants :refer :all]
            [econometrics.integrals :refer :all]
            [econometrics.curves :refer :all]
            [econometrics.dummy-data :refer :all]))

(defmacro zip
  [& args]
  `(partition ~(count args) (interleave ~@args)))

(defn mean
  "Average of a sequence"
  ([] nil)
  ([xs]
   (float (/ (reduce + xs) (count xs)))))

(defn median
  "Median value of a sequence"
  ([] nil)
  ([xs]
   (let [xs (sort xs)
         mid (int (/ (count xs) 2))]
     (if (odd? (count xs))
       (float (nth xs mid))
       (float (/ (+ (nth xs (dec mid)) (nth xs mid)) 2))))))

(defn range-stat
  "Range between the largest and smallest values in a sequence"
  ([] nil)
  ([xs]
   (let [xs (sort xs)]
     (float (- (last xs) (first xs))))))

(defn partition-by-values
  "Returns a list partitioned into sublists of distinct values"
  ([] nil)
  ([xs]
   (partition-by identity (sort xs))))

(defn mode
  "Finds the most common value in the set"
  ([] nil)
  ([xs]
   (let [xs (partition-by identity (sort xs))]
     (loop [[head & tail :as all] xs
            top '()]
       (if (empty? all)
         (first top)
         (if (> (count head) (count top))
           (recur tail head)
           (recur tail top)))))))

(defn abs-errors
  "Finds the absolute values of the error terms"
  [xs]
  (let [m (mean xs)]
    (map #(Math/abs (- % m)) xs)))

(defn average-abs-error
  "Finds the average of the absolute errors"
  [xs]
  (mean (abs-errors xs)))

(defn error-terms
  "Finds the raw error terms for each item in the sequence"
  [xs]
  (let [mean (mean xs)]
    (map #(- % mean) xs)))

(defn dot-errors
  "Finds the dot product of the error terms for pairs of xs and ys"
  [xs ys]
  (dot-product (error-terms xs) (error-terms ys)))

(defn squared-errors
  "Finds the squared error term for each item in the sequence"
  [xs]
  (map square (error-terms xs)))

(defn sum-squared-errors
  "Finds the sum of squared deviations for a sequence"
  [xs]
  (reduce + (squared-errors xs)))

(defn population-variance
  "Finds the population variance of a sequence"
  [xs]
  (/ (sum-squared-errors xs) (count xs)))

(defn sample-variance
  "Finds the sample variance of a sequence"
  [xs]
  (let [n (- (count xs) 1)]
    (/ (sum-squared-errors xs) n)))

(defn population-sigma
  "Finds the standard deviation of a sequence using the population formula"
  [xs]
  (sqrt (population-variance xs)))

(defn sample-sigma
  "Finds the standard deviation of a sequence using the sample forumla"
  [xs]
  (sqrt (sample-variance xs)))

(defn population-z-scores
  "Returns a sequence of all the z-scores for each item in the sequence.
  Uses the sigma formula for the population"
  [xs]
  (let [errors (error-terms xs)
        sigma (population-sigma xs)]
    (map #(/ % sigma) errors)))

(defn sample-z-scores
  "Returns a sequence of all the z-scores for each item in the sequence.
  Uses the sigma formula for a sample"
  [xs]
  (let [errors (error-terms xs)
        sigma (sample-sigma xs)]
    (map #(/ % sigma) errors)))

(defn population-z-scores-map
  "Returns a map with the raw score as the key and the z-score as the value"
  [xs]
  (let [z-scores (population-z-scores xs)]
    (loop [fin {}
           n 0]
      (if (> n (dec (count xs)))
        fin
        (recur (assoc fin (nth xs n) (nth z-scores n)) (inc n))))))

(defn sample-z-scores-map
  "Returns a map with the raw score as the key and the z-score as the value"
  [xs]
  (let [z-scores (sample-z-scores xs)]
    (loop [fin {}
           n 0]
      (if (> n (dec (count xs)))
        fin
        (recur (assoc fin (nth xs n) (nth z-scores n)) (inc n))))))

(defn population-mean-std-error
  "Calculating the standard error of the mean for the population"
  [xs]
  (/ (population-sigma xs) (sqrt (count xs))))

(defn sample-mean-std-error
  "Calculating the standard error of the mean for a sample"
  [xs]
  (/ (sample-sigma xs) (sqrt (count xs))))

(defn population-covariance
  "Finds the covariance of two sequences with the population formula"
  [xs ys]
  (mean (dot-errors xs ys)))

(defn sample-covariance
  "Finds the covariance between two sequences using the sample formula"
  [xs ys]
  (/ (reduce + (dot-errors xs ys))
     (dec (count xs))))

(defn population-correlation-coefficient
  "Finds the correlation coefficient for a population"
  [xs ys]
  (/ (population-covariance xs ys)
     (* (population-sigma xs) (population-sigma ys))))

(defn sample-correlation-coeff
  "Finds the correlation coefficient for a sample"
  [xs ys]
  (/ (sample-covariance xs ys)
     (* (sample-sigma xs) (sample-sigma ys))))

(defn sample-t-value
  "Finds the t value for a single sample"
  [xs]
  (let [r (sample-correlation-coeff xs)
        n (count xs)]
    (* r (sqrt (/ (- n 2)
                  (- 1 (square r)))))))

(defn coefficient-of-determination
  "Finds the coefficient of determination between two samples"
  [xs ys]
  (square (sample-correlation-coeff xs ys)))

(defn sample-std-err-of-the-mean
  "Finds the standard error of the mean for a sample"
  [xs]
  (/ (sample-sigma xs) (sqrt (count xs))))

(defn std-err-diff-between-means
  "Finds the std error of the difference between the means for two samples"
  [xs ys]
  (sqrt (+ (square (sample-std-err-of-the-mean xs))
           (square (sample-std-err-of-the-mean ys)))))

(defn independent-samples-t-test
  "Finds the t-test value for two independent samples of the same length"
  [xs ys]
  (/ (- (mean xs) (mean ys)) (std-err-diff-between-means xs ys)))

(defn independent-samples-effect-size
  "Finds the effect size for an independent samples t-test"
  [xs ys]
  (let [s (fn [xs ys] (* (sqrt (count xs))
                         (std-err-diff-between-means xs ys)))]
    (/ (- (mean xs) (mean ys))
       (s xs ys))))

(defn one-tailed-p-value-from-z-test
  "Finds the one tailed p-value given a z test"
  [z]
  (integral std-normal-distribution z 10 1000))

(defn two-tailed-p-value-from-z-test
  "Finds the two tailed p-value given a z-test"
  [z]
  (* 2 (one-tailed-p-value-from-z-test z)))

(defn one-tailed-p-value-from-t-test
  "Finds the one-tailed p-value given a t-test t, and degrees of freedom v"
  [v t]
  (integral (partial t-distribution v) t 10 1000))

(defn two-tailed-p-value-from-t-test
  "Finds the two-tailed p-value given a t-test t, and degrees of freedom v"
  [v t]
  (* 2 (one-tailed-p-value-from-t-test v t)))

