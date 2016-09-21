(ns cml.utils.variation
  (:require [cml.utils.central-tendancy :refer [mean -mean mean-1 -mean-1]]
            [uncomplicate.neanderthal.native :as nn]
            [uncomplicate.neanderthal.core :as n]
            [cml.utils.primitive :refer [pdiv pminus ptimes pplus]]
            [uncomplicate.commons.core :refer [with-release let-release]]
            [cml.utils.vector :refer [vminus]])
  (:use [uncomplicate.fluokitten core jvm]))

;TODO remove protocol for plain functions
;TODO implements as BLAS functions
;TODO change return types to be the calculation not map

(defprotocol Variation
  (standard-deviation [sd] "Standard deviation")
  (variance [v] "Variance"))


(defn -smpl-std-dev [data mean]
  "Computes the sample standard deviation"
  (with-release [means (vminus (n/entry! (nn/dv data) mean) data)]
                (Math/sqrt (-mean-1 (fmap! ptimes means means)))))

(defn -pop-std-dev [data mean]
  "Computes the population standard deviation"
  (with-release [means (vminus (n/entry! (nn/dv data) mean) data)]
                (Math/sqrt (-mean (fmap! ptimes means means)))))

(defn -smpl-var [data mean]
  "Computes the sample variance"
  (/ (n/dot (vminus (n/entry! (nn/dv data) mean) data) data)
     (dec (n/ecount data))))

(defn -pop-var [data mean]
  "Computes the population variance"
  (/ (n/dot (vminus (n/entry! (nn/dv data) mean) data) data)
     (n/ecount data)))

(defn -pool-var [data mean]
  "Computes the pooled variance"
  (let-release [means  (vminus (n/entry! (nn/dv data) mean) data)]
               (let [ec (dec (n/ecount data))]
                 (/ (* ec (/ (n/dot means data) ec)) ec))))


(defrecord Sample [sample-mean sample]
  Variation

  (standard-deviation [type]
    (assoc type :standard-deviation
                (Math/sqrt (mean-1 (map #(* (- sample-mean %)
                                            (- sample-mean %))
                                        sample)))))

  (variance [type]
    (assoc type :variance
                (/ (reduce + (map #(* (- % sample-mean)
                                      (- % sample-mean)) sample))
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
