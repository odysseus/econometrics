(ns econometrics.graveyard)

;; Like Elephants, this is where code goes when it knows its time is up

;; This one doesn't quite work. Has problems with integrals of exponential
;; decay because it sets the ymax wrong, and has problems with integrals
;; of ranges greater than 0 to 1 for reasons I still need to debug. Not to
;; mention it's both vastly slower and less accurate than the calculation
;; methods. Keeping it around because it was too much work to just delete.
(defn monte-carlo-integral
  "Finds an integral using a monte carlo method"
  [f a b n]
  (let [rand-float-in-range (fn [a b] (+ (rand (- b a)) a))
        xmin a
        xmax b
        ymin 0
        ymax (f b)
        randx (fn [] (rand-float-in-range xmin xmax))
        randy (fn [] (rand-float-in-range ymin ymax))
        above-curve (fn [x y] (> y (f x)))]
    (loop [above 0
           below 0
           c 0]
      (if (>= c n)
        (float (/ below (+ below above)))
        (if (above-curve (randx) (randy))
          (recur (inc above) below (inc c))
          (recur above (inc below) (inc c)))))))
