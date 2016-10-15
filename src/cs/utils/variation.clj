(ns cs.utils.variation
  (:require [cs.utils.central-tendancy :refer [mean -mean mean-1 -mean-1]]
            [uncomplicate.neanderthal.native :as nn]
            [uncomplicate.neanderthal.core :as n]
            [cs.utils.primitive :refer [pdiv pminus ptimes pplus]]
            [uncomplicate.commons.core :refer [with-release]]
            [cs.utils.vector :refer [vminus]])
  (:use [uncomplicate.fluokitten core jvm]))

;TODO move into single utils file

(defprotocol Variation
  (standard-deviation [sd] "Standard deviation")
  (variance [v] "Variance"))


(defn smpl-std-dev [data mean]
  "Computes the sample standard deviation"
  (Math/sqrt (mean-1 (map #(* (- mean %)
                              (- mean %)) data))))

(defn pop-std-dev [data mean]
  "Computes the population standard deviation"
  (Math/sqrt (mean (map #(* (- mean %) (- mean %)) data))))

(defn -smpl-var [data mean]
  "Computes the sample variance"
  (/ (n/dot (vminus (n/entry! (nn/dv data) mean) data) data)
     (dec (n/ecount data))))

(defn -pop-var [data mean]
  "Computes the population variance"
  (/ (n/dot (vminus (n/entry! (nn/dv data) mean) data) data)
     (n/ecount data)))

(defn pool-var [data mean size-minus-one]
  "Computes the pooled variance"
  (/ (* size-minus-one
        (/ (reduce + (map #(* (- % mean) (- % mean)) data))
           size-minus-one)) size-minus-one))




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
