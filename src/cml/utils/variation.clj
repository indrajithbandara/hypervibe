(ns cml.utils.variation
  (:require [cml.utils.central-tendancy :refer [mean -mean mean-1 -mean-1]]
            [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal]
            [cml.utils.primitive :refer [pdiv pminus ptimes pplus]]
            [uncomplicate.commons.core :refer [with-release]]
            [cml.utils.vector :refer [vminus]])
  (:use [uncomplicate.fluokitten core jvm]))

;TODO remove protocol for plain functions
;TODO implements as BLAS functions
;TODO change return types to be the calculation not map

(defprotocol Variation
  (standard-deviation [sd] "Standard deviation")
  (variance [v] "Variance"))


(defn -standard-deviation [data-mean data]
  (Math/sqrt (-mean-1 (neanderthal/dot                      ;TODO wrong
                        (vminus (neanderthal/entry! data data-mean) data)
                        (vminus (neanderthal/entry! data data-mean) data)))))


(defrecord Sample [sample-mean sample]
  Variation

  (standard-deviation [type]
    (assoc type :standard-deviation
                (Math/sqrt (mean-1 (map #(* (- sample-mean %)
                                            (- sample-mean %))
                                        sample)))))

  (variance [type]
    (assoc type :variance
                (/ (reduce + (map #(* (- % sample-mean) (- % sample-mean)) sample))
         (dec (count sample))))))


(defrecord Population [population-mean population]
  Variation

  (standard-deviation [type]
    (assoc type :standard-deviation
                (Math/sqrt (mean (map #(* (- population-mean %) (- population-mean %)) population)))))

  (variance [type]
    (assoc type :variance
                (/ (reduce + (map #(* (- % population-mean) (- % population-mean)) population))
                   (count population)))))


(defrecord Pooled [pooled-mean pooled-data size-1]
  Variation

  (variance [type]
    (assoc type :variance
                (/ (* size-1 (/ (reduce + (map #(* (- % pooled-mean) (- % pooled-mean)) pooled-data))
                                (dec (count pooled-data)))) size-1))))

;(mean [3 6 3 7 5 44 6 7]) => 10.125

(def d1 (neanderthal-native/dv [1.0 2.0 3.0]))
(def d2 (neanderthal-native/dv [4.0 5.0 6.0]))
(def d3 (neanderthal-native/dv [1.0 2.0 3.0 4.0 5.0]))
(def d4 (neanderthal-native/dv [3.0 6.0 3.0 7.0 5.0 44.0 6.0 7.0]))
(def d99 (neanderthal-native/dv [3.0 6.0 3.0 7.0 5.0 44.0 6.0 7.0]))
(def d5 (neanderthal/entry! d99 10.125))
(def d6 (neanderthal-native/dv [9 5 3 5 66 77 8 7]))