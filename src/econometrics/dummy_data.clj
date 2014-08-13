(ns econometrics.dummy-data
  (:require [econometrics.constants :refer :all]
            [econometrics.curves :refer :all]))

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

(defn rand-float-in-range
  "Generates a random float in the range a to b"
  [a b]
  (+ (rand (- b a)) a))

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

(defn normally-distributed-sequence
  "Using the dice and some clever math this creates a normally distributed
  sequence with mean mu and standard-deviation s"
  [mu s samples]
  (let [curve (partial normal-distribution mu s)
        xrange (* s 10)
        rx #(rand-float-in-range (+ (- xrange) mu) (+ xrange mu))
        ry #(rand-float-in-range 0 1)]
    (loop [x (rx)
           y (ry)
           fin '()]
      (if (>= (count fin) samples)
        fin
        (if (<= y (curve x))
          (recur (rx) (ry) (cons x fin))
          (recur (rx) (ry) fin))))))


