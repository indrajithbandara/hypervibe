(ns clojure.core.stats.lom.interval.test
  (:require [clojure.stats.lom.interval.test :refer [ttest]])
  (:import [clojure.stats.lom.interval.test OneSample EqualVariance Welch RepeatedMeasure]))

;TODO seperate out tests into differnt packages depending on the data types eg numerical/categorocal etc

(defn one-smpl-ttest

  "Computes a students t-test for testing the null hypothesis that the population mean, is equal to a hypothesised mean. The
   students one sample t-test requires a sample that is normally distributed"

  [{sample :sample h-mean :h-mean alpha :alpha :or {alpha 0.05} tail :tail}]
  (ttest (OneSample. sample h-mean alpha tail)))

;TODO add extensive docs, specs
(defn equal-var-ttest [{samples :samples h-mean :h-mean alpha :alpha :or {alpha 0.05} tail :tail}]
  (ttest (EqualVariance. samples h-mean alpha tail)))

;TODO add extensive docs, specs
(defn welch-ttest [{samples :samples alpha :alpha :or {alpha 0.05}}]
  (ttest (Welch. samples alpha)))

;TODO add extensive docs, specs
(defn rep-msure-ttest [{population :population h-mean :h-mean alpha :alpha :or {alpha 0.05} tail :tail}]
  (ttest (RepeatedMeasure. population h-mean alpha tail)))
