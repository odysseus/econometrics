(ns econometrics.core
  (:require [clojure.core.reducers :as r])
  (:gen-class))

(defn square [x] (* x x))

(defmacro zip
  [& args]
  `(partition ~(count args) (interleave ~@args)))

(defn randn
  "Generates a random number in the range 0 to a"
  [a]
  (int (* (rand 1) a)))

(defn rand-in-range
  "Generates a random integer in the range a to b"
  [a b]
  (if (< a b)
    (+ (randn (- b a)) a)
    (+ (randn (- a b)) b)))

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
   (float (/ (r/fold + xs) (count xs)))))

(defn median
  "Median value of a sequence"
  ([] nil)
  ([sq]
   (let [xs (sort sq)
         mid (int (/ (count xs) 2))]
   (if (odd? (count xs))
     (nth xs mid)
     (float (/ (+ (nth xs (dec mid)) (nth xs mid)) 2))))))

(defn range-stat
  "Range between the largest and smallest values in a sequence"
  ([] nil)
  ([sq]
   (let [xs (sort sq)]
     (- (last xs) (first xs)))))

(defn partition-by-values
  "Returns a list partitioned into sublists of distinct values"
  ([] nil)
  ([sq]
   (partition-by #(identity %) (sort sq))))

(defn mode
  "Finds the most common value in the set"
  ([] nil)
  ([sq]
   (let [xs (partition-by #(identity %) (sort sq))]
     (loop [[head & tail :as all] xs
            top '()]
       (if (empty? all)
         (first top)
         (if (> (count head) (count top))
           (recur tail head)
           (recur tail top)))))))

(defn error-terms
  "Finds the raw error terms for each item in the sequence"
  [sq]
  (let [mean (mean sq)]
    (map #(- % mean) sq)))

(defn squared-errors
  "Finds the squared error term for each item in the sequence"
  [sq]
  (map square (error-terms sq)))

(defn sum-of-squares
  "Finds the sum of squared deviations for a sequence"
  [sq]
  (r/fold + (squared-errors sq)))

(defn population-variance
  "Finds the population variance of a sequence"
  [sq]
  (/ (sum-of-squares sq) (count sq)))

(defn sample-variance
  "Finds the sample variance of a sequence"
  [sq]
  (let [n (- (count sq) 1)]
    (/ (sum-of-squares sq) n)))

(defn population-sigma
  "Finds the standard deviation of a sequence using the population formula"
  [sq]
  (Math/sqrt (population-variance sq)))

(defn sample-sigma
  "Finds the standard deviation of a sequence using the sample forumla"
  [sq]
  (Math/sqrt (sample-variance sq)))

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

(defn -main
  [& args]
  (def x (random-sequence 10 1 10))
  (println (rand-in-range 1 10))
  (println x)
  (println (sample-z-scores-map x)))
