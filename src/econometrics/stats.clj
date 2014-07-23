(ns econometrics.stats
  (:require [clojure.core.reducers :as r]))

(defn square [x] (* x x))
(defn sqrt [n] (Math/sqrt n))
(defn pow [a b] (Math/pow a b))

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

(defn mean
  "Average of a sequence"
  ([] nil)
  ([xs]
   (float (/ (reduce + xs) (count xs)))))

(defn median
  "Median value of a sequence"
  ([] nil)
  ([sq]
   (let [xs (sort sq)
         mid (int (/ (count xs) 2))]
   (if (odd? (count xs))
     (float (nth xs mid))
     (float (/ (+ (nth xs (dec mid)) (nth xs mid)) 2))))))

(defn range-stat
  "Range between the largest and smallest values in a sequence"
  ([] nil)
  ([sq]
   (let [xs (sort sq)]
     (float (- (last xs) (first xs))))))

(defn partition-by-values
  "Returns a list partitioned into sublists of distinct values"
  ([] nil)
  ([sq]
   (partition-by identity (sort sq))))

(defn mode
  "Finds the most common value in the set"
  ([] nil)
  ([sq]
   (let [xs (partition-by identity (sort sq))]
     (loop [[head & tail :as all] xs
            top '()]
       (if (empty? all)
         (first top)
         (if (> (count head) (count top))
           (recur tail head)
           (recur tail top)))))))

(defn abs-errors
  "Finds the absolute values of the error terms"
  [sq]
  (let [m (mean sq)]
    (map #(Math/abs (- % m)) sq)))

(defn average-abs-error
  "Finds the average of the absolute errors"
  [sq]
  (mean (abs-errors sq)))

(defn error-terms
  "Finds the raw error terms for each item in the sequence"
  [sq]
  (let [mean (mean sq)]
    (map #(- % mean) sq)))

(defn squared-errors
  "Finds the squared error term for each item in the sequence"
  [sq]
  (map square (error-terms sq)))

(defn sum-squared-errors
  "Finds the sum of squared deviations for a sequence"
  [sq]
  (reduce + (squared-errors sq)))

(defn population-variance
  "Finds the population variance of a sequence"
  [sq]
  (/ (sum-squared-errors sq) (count sq)))

(defn sample-variance
  "Finds the sample variance of a sequence"
  [sq]
  (let [n (- (count sq) 1)]
    (/ (sum-squared-errors sq) n)))

(defn population-sigma
  "Finds the standard deviation of a sequence using the population formula"
  [sq]
  (sqrt (population-variance sq)))

(defn sample-sigma
  "Finds the standard deviation of a sequence using the sample forumla"
  [sq]
  (sqrt (sample-variance sq)))

(defn population-z-scores
  "Returns a sequence of all the z-scores for each item in the sequence.
  Uses the sigma formula for the population"
  [sq]
  (let [errors (error-terms sq)
        sigma (population-sigma sq)]
    (map #(/ % sigma) errors)))

(defn sample-z-scores
  "Returns a sequence of all the z-scores for each item in the sequence.
  Uses the sigma formula for a sample"
  [sq]
  (let [errors (error-terms sq)
        sigma (sample-sigma sq)]
    (map #(/ % sigma) errors)))

(defn population-z-scores-map
  "Returns a map with the raw score as the key and the z-score as the value"
  [sq]
  (let [z-scores (population-z-scores sq)]
    (loop [fin {}
           n 0]
      (if (> n (dec (count sq)))
        fin
        (recur (assoc fin (nth sq n) (nth z-scores n)) (inc n))))))

(defn sample-z-scores-map
  "Returns a map with the raw score as the key and the z-score as the value"
  [sq]
  (let [z-scores (sample-z-scores sq)]
    (loop [fin {}
           n 0]
      (if (> n (dec (count sq)))
        fin
        (recur (assoc fin (nth sq n) (nth z-scores n)) (inc n))))))

(defn population-mean-std-error
  "Calculating the standard error of the mean for the population"
  [sq]
  (/ (population-sigma sq) (sqrt (count sq))))

(defn sample-mean-std-error
  "Calculating the standard error of the mean for a sample"
  [sq]
  (/ (sample-sigma sq) (sqrt (count sq))))

(defn sample-pearson-product-moment-correlation-coefficient
  "Finds the Pearson product-moment correlation coefficient for a sample"
  [xs ys]
  (/ (reduce + (map * (error-terms xs) (error-terms ys)))
     (* (sqrt (sum-squared-errors xs)) (sqrt (sum-squared-errors ys)))))

(defn pearson-correlational-coefficient
  "Pearson correlational coefficient - another formula"
  [xs ys]
  (/ (reduce + (map * (population-z-scores xs) (population-z-scores ys)))
     (if (> (count xs) (count ys))
       (count xs)
       (count ys))))
