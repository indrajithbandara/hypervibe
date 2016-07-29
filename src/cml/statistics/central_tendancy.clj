(ns cml.statistics.central-tendancy
  (:use [uncomplicate.neanderthal core native]
        [uncomplicate.fluokitten core jvm]
        [criterium.core])
  (:require [cml.utils :refer [double-asum]]))
(use 'criterium.core)

(defn ^double mean [data] (/ (reduce + data) (count data)))

(defn ^double mean-1 [data] (/ (reduce + data) (dec (count data))))

(defn difference [[sample-one sample-two]] (map - sample-one sample-two))


(defn permutations
  [x xs]                                                    ;TODO Put in respectable NS
  (letfn [(factorial [x]
            (loop [cnt (if (coll? x)
                         (count x) x) acc 1]
              (if (zero? cnt)
                acc (recur (dec cnt) (*' cnt acc)))))]
    (quot (factorial x)
          (factorial (- x xs)))))


(defn significance [correlation sample-size]
  (/ (* correlation
        (Math/sqrt sample-size))
     (Math/sqrt (- 1 (* correlation correlation)))))

