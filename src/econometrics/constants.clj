(ns econometrics.constants)

(def e (Math/E))
(def pi (Math/PI))

(defn square [x] (* x x))
(defn sqrt [n] (Math/sqrt n))
(defn pow [a b] (Math/pow a b))
(defn dot-product [xs ys] (map * xs ys))
(defn twinc [x] (+ x 2))
(def sqrt-of-pi 1.772453850905)
