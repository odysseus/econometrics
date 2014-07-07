(ns econometrics.core
  (:gen-class))

(defn square [x] (* x x))

(defn randn
  "Generates a random number in the range 0 to a"
  [a]
  (int (* (rand 1) a)))

(defn rand-in-range
  "Generates a random integer in the range a to b"
  [a b]
  (if (> a b)
    (+ (randn (- a b)) a)
    (+ (randn (- b a)) b)))

(defn generate-sequence
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

(defn sigma
  "Finds the standard deviation of a sequence"
  ([] nil)
  ([sq]
   (let [xs (sort sq)
         avg (mean sq)]
     (Math/sqrt (mean (map #(square (- % avg)) xs))))))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def x (generate-sequence 100 5 15))
  (println (mean x))
  (println (median x))
  (println (range-stat x))
  (println (mode x))
  (println (sigma x)))

