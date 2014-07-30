(ns econometrics.stats
  (:require [clojure.core.reducers :as r]))

(defn square [x] (* x x))
(defn sqrt [n] (Math/sqrt n))
(defn pow [a b] (Math/pow a b))
(defn dot-product [xs ys] (map * xs ys))

(defmacro zip
  [& args]
  `(partition ~(count args) (interleave ~@args)))

(defn randn
  "Generates a random integer in the range 0 to a"
  [a]
  (int (* (rand 1) a)))

(defn round [n places]
  (let [times (pow 10 places)]
    (/ (Math/round (* n times)) times)))

(defn rand-in-range
  "Generates a random integer in the range a to b inclusive"
  [a b]
  (if (< a b)
    (+ (rand-int (inc (- b a))) a)
    (+ (rand-int (inc (- a b))) b)))

(defn random-sequence
  "Generates a sequence of n random items in the range of a to b"
  [n a b]
  (loop [c n
         fin '()]
    (if (= c 0)
      fin
      (recur (dec c) (cons (rand-in-range a b) fin)))))

(defn random-sequence-increasing
  "Calculates a random sequence whose values should trend upwards
  use a large set or a small range to get the most defined trend"
  [n r s]
  (loop [current 0
         lower 0
         upper r
         fin '()]
    (if (= current n)
      (reverse fin)
      (recur (inc current)
             (+ lower s)
             (+ upper s)
             (cons (rand-in-range lower upper) fin)))))

(defn d6
  "Simulates a d6 dice roll"
  []
  (rand-in-range 1 6))

(defn xd6
  "Returns the total from rolling x d6 dice"
  [x]
  (reduce + (take x (repeatedly d6))))

(defn xd6-sequence
  "Conducts n rolls of x d6 dice and puts the totals into a list"
  [n x]
  (take n (repeatedly #(xd6 x))))

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
  "Finds the t value for a sample"
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

(defn independent-sample-t-test
  "Finds the t-test value for two independent samples of the same length"
  [xs ys]
  (/ (- (mean xs) (mean ys)) (std-err-diff-between-means xs ys)))

(def sample-size 10)
(def xs (xd6-sequence sample-size 5))
(def ys (xd6-sequence sample-size 5))

(println (independent-sample-t-test xs ys))
