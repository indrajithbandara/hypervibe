(ns clojure.core.stats.lom.interval.test
  (:require [clojure.stats.lom.interval.test :refer [ttest]])
  (:import [clojure.stats.lom.interval.test OneSample EqualVariance Welch RepeatedMeasure]))

;TODO seperate out tests into differnt packages depending on the data types eg numerical/categorocal etc

(defn one-smpl-ttest

  "Computes a students t-test for testing the null hypothesis that the population mean, is equal to a hypothesised mean. The
   students one sample t-test requires a sample that is normally distributed"

  [{sample :sample h-mean :h-mean alpha :alpha :or {alpha 0.05}}]
  (ttest (OneSample. sample h-mean alpha)))

;TODO add extensive docs, specs
(defn equal-var-ttest [{:keys [samples h-mean]}]
  (ttest (EqualVariance. samples h-mean)))

;TODO add extensive docs, specs
(defn welch-ttest [{:keys [samples]}]
  (ttest (Welch. samples)))

;TODO add extensive docs, specs
(defn rep-msure-ttest [{:keys [population h-mean]}]
  (ttest (RepeatedMeasure. population h-mean)))
