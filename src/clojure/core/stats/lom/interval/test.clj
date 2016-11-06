(ns clojure.core.stats.lom.interval.test
  (:require [clojure.stats.lom.interval.test :refer [ttest]])
  (:import [clojure.stats.lom.interval.test OneSample EqualVariance Welch RepeatedMeasure]))

;TODO seperate out tests into differnt packages depending on the data types eg numerical/categorocal etc

(defn one-smpl-ttest

  "A one sample t-test is a hypothesis test, in which the test statistic follows a Student's t-distribution under the null hypothesis. It can be used to determine if
   two sets of data are significantly different from each other

  INPUT:

    1) {:smpl <sequential collection> :h-mean <long>}

    2) {:smpl <sequential collection> :h-mean <long> :alpha <double>}

  OUTPUT:

   {:smpl <sequential collection>
    :h-mean <long>
    :alpha <double>
    :t-stat <double>
    :dof <long>
    :crtcl-value <double>
    :smpl-mean <double>
    :smpl-std-dev <double>
    :smpl-size <long>}

  DETAILS:

    :smpl = sample data
    :h-mean = hypothesized mean
    :alpha = alpha value (percentile)
    :t-stat = test statistic
    :dof = degrees of freedom
    :crtcl-value = critical value
    :smpl-mean = sample mean
    :smpl-std-dev =  sample standard deviation
    :smpl-size = sample size

  NOTE:

    Alpha value defaults to 0.05 unless explicitly defined
   "

  [{sample :smpl h-mean :h-mean alpha :alpha :or {alpha 0.05}}]
  (ttest (OneSample. sample h-mean alpha)))

;TODO add extensive docs, specs
(defn equal-var-ttest [{samples :samples h-mean :h-mean alpha :alpha :or {alpha 0.05}}]
  (ttest (EqualVariance. samples h-mean alpha)))

;TODO add extensive docs, specs
(defn welch-ttest [{samples :samples alpha :alpha :or {alpha 0.05}}]
  (ttest (Welch. samples alpha)))

;TODO add extensive docs, specs
(defn rep-msure-ttest [{population :population h-mean :h-mean alpha :alpha :or {alpha 0.05}}]
  (ttest (RepeatedMeasure. population h-mean alpha)))


