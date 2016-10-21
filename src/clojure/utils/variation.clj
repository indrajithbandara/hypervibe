(ns clojure.utils.variation
  (:require [clojure.utils.central-tendancy :refer [mean -mean mean-1 -mean-1]]
            [uncomplicate.neanderthal.native :as nn]
            [uncomplicate.neanderthal.core :as n]
            [clojure.utils.primitive :refer [pdiv pminus ptimes pplus]]
            [uncomplicate.commons.core :refer [with-release]]
            [clojure.utils.vector :refer [vminus]])
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

(defn pop-var [data mean]
  "Computes the population variance"
  (/ (reduce + (map #(* (- % mean) (- % mean)) data))
     (count data)))

(defn smpl-var [data mean]
  "Computes the sample variance"
  (/ (reduce + (map #(* (- % mean) (- % mean)) data))
     (dec (count data))))

(defn pool-var [data mean size-minus-one]
  "Computes the pooled variance"
  (/ (* size-minus-one
        (/ (reduce + (map #(* (- % mean) (- % mean)) data))
           size-minus-one)) size-minus-one))


