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


(defn -standard-deviation [data-mean data]                  ;TODO reimplement to slow.. get rid of (repeat ..)
  (with-release [data-means (neanderthal-native/dv (repeat (neanderthal/ecount data) data-mean))
                 minus-data-means (vminus data-means (neanderthal-native/dv data))]
                (Math/sqrt (-mean-1 (fmap! ptimes minus-data-means minus-data-means)))))


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


