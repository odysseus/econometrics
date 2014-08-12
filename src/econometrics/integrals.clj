(ns econometrics.integrals
  (:require [econometrics.constants :refer :all]))

(defn fn-range-sum
  "Takes a range a to b and two functions, f generates the values to be
  summed and nx generates the next value in the sequence"
  [f nx a b]
  (loop [c a
         tot 0]
    (if (> c b)
      tot
      (recur (nx c) (+ tot (f c))))))

(defn simple-integral
  "Simple, slow and less accurate method for calculating integrals.
  This is retained because its simplicity makes it good for checking values
  against the other integral checking methods below"
  [f a b dx]
  (* (fn-range-sum f #(+ % dx) (+ a (/ dx 2.0)) b)
     dx))

(defn integral
  "Calculates an integral using Simpson's method.
  f is the function of the curve
  a and b are the range of the integral
  n controls the number of samples measured
  and thus the accuracy of the calculation."
  [f a b n]
  (let [h (/ (- b a) n)
        y (fn [k] (f (+ a (* k h))))]
    (float
      (* (/ h 3)
         (+ (y n) (y 0)
            (reduce + (map #(* 4 (y %)) (range 1 n 2)))
            (reduce + (map #(* 2 (y %)) (range 2 n 2))))))))
