(ns clojure.core.stats.lom.interval.test
  (:require [clojure.stats.lom.interval.test :refer [ttest]]
            [clojure.stats.lom.categorical.test :refer [pearson-chi-square]]
            [clojure.utils.variation :refer [standard-deviation variance]]
            [clojure.utils.central-tendancy :refer [mean difference]])
  (:import [clojure.stats.lom.interval.test OneSample EqualVariance Welch RepeatedMeasure]))

;TODO seperate out tests into differnt packages depending on the data types eg numerical/categorocal etc

(defn one-sample-ttest
  "Computes a students t-test for testing the null hypothesis that the population mean, is equal to a hypothesised mean"
  [{:keys [sample h-mean]}]
  (ttest (OneSample. sample h-mean)))


(defn equal-var-ttest [{:keys [samples h-mean]}]
  (ttest (EqualVariance. samples h-mean)))


(defn welch-ttest [{:keys [samples]}]
  (ttest (Welch. samples)))


(defn rep-measure-ttest [{:keys [population h-mean]}]
  (ttest (RepeatedMeasure. population h-mean)))


