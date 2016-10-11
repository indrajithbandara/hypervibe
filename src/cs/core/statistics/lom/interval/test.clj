(ns cs.core.statistics.lom.interval.test
  (:require [cs.statistics.lom.interval.test :refer [ttest]]
            [cs.statistics.lom.categorical.test :refer [pearson-chi-square]]
            [cs.utils.variation :refer [standard-deviation variance]]
            [cs.utils.central-tendancy :refer [mean difference]])
  (:import [cs.statistics.lom.interval.test OneSample EqualVariance Welch RepeatedMeasure]))

;TODO seperate out tests into differnt packages depending on the data types eg numerical/categorocal etc

(defn one-sample-ttest
  "Computes a students t-test for testing the null hypothesis that the population mean, is equal to a hypothesised mean"
  [{:keys [sample h-mean]}]
  (ttest (OneSample. sample h-mean)))


(defn equal-var-ttest [{:keys [sample h-mean]}]
  (ttest (EqualVariance. sample h-mean)))


(defn welch-ttest [{:keys [sample]}]
  (ttest (Welch. sample)))


(defn rep-measure-ttest [{:keys [population h-mean]}]
  (ttest (RepeatedMeasure. population h-mean)))



