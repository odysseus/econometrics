(ns econometrics.curves
  (:require
    [econometrics.constants :refer :all]
    [econometrics.integrals :refer :all]))

(defn- gamma-whole
  "Gamma function for whole numbers"
  [n]
  (reduce * (range 1 n)))

(defn- gamma-half
  "Gamma function for half numbers"
  [n]
  (loop [cur (- n 1) tot 1]
    (if (< cur 0)
      (* sqrt-of-pi tot)
      (recur (- cur 1) (* tot cur)))))

(defn gamma
  "This gamma function works only for whole numbers and numbers ending in .5,
  fortunately these are the only two versions we will encounter in the t curve"
  [n]
  (if (or (= (type n) java.lang.Double) (= (type n) clojure.lang.Ratio))
    (gamma-half n)
    (gamma-whole n)))

(defn t-distribution
  "Equation for Student's t distribution, f(t) with k degrees of freedom"
  [k t]
  (let [a (gamma (/ (+ k 1) 2))
        b (* (Math/sqrt (* k pi)) (gamma (/ k 2)))
        c (+ 1 (/ (square t) k))
        d (- (/ (+ k 1) 2))]
    (* (/ a b) (Math/pow c d))))

(defn normal-distribution
  "Equation for the normal distribution f(x) with mean mu and std-dev sigma"
  [mu sigma x]
  (let [a (* sigma (Math/sqrt (* 2 pi)))
        b (- (/ (square (- x mu))
                (* 2 (square sigma))))]
    (* (/ 1 a) (Math/pow e b))))

(defn std-normal-distribution
  "Creates a curve for the normal distribution with mean 0 and
  variance/std-dev of 1"
  [n]
  (normal-distribution 0 1 n))
