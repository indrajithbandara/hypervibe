(ns cml.utils.central-tendancy
  (:require [cml.utils :refer [double-asum]]
            [clojure.core.reducers :as r]
            [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal]
            [cml.utils.primitive :refer [pdiv pminus ptimes pplus]]
            [uncomplicate.commons.core :refer [with-release]]
            [cml.utils.vector :refer [vminus]])
  (:use [uncomplicate.fluokitten core jvm]))
(use 'criterium.core)

;TODO implement BLAS versions of functions for interval tests re implementation

(defn -mean ^double [data] (pdiv (neanderthal/sum data) (neanderthal/ecount data)))

(defn ^double mean [data] (/ (r/fold + data) (count data)))

(defn ^double mean-1 [data] (/ (r/fold + data) (dec (count data))))

(defn -mean-1 ^double [data] (pminus (pdiv (neanderthal/sum data) (neanderthal/ecount data)) 1))

(defn ^doubles difference [[sample-one sample-two]] (map - sample-one sample-two))

(defn -difference [vec-one vec-two] (vminus vec-one vec-two))


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


