(ns clojure.core.stats.lom.interval.test
  (:require [clojure.stats.lom.interval.test :refer [ttest]])
  (:import [clojure.stats.lom.interval.test OneSample EqualVariance Welch RepeatedMeasure]))
(use 'criterium.core)

(defn one-smpl-ttest

  "One sample t-test is a hypothesis test, in which the test statistic follows
   a Student's t-distribution under the null hypothesis. It can be
   used to determine if two sets of data are significantly
   different from each other.

   INPUT:

    1) {:smpl <list[num] | vector[num] | seq[num]> :h-mean <num>}

    2) {:smpl <list[num] | vector[num] | seq[num]> :h-mean <num> :alpha <num>}

    Possible alpha values:

    0.40, 0.30, 0.20, 0.15, 0.10, 0.05, 0.025, 0.02, 0.015, 0.01, 0.0075, 0.005, 0.0025, 0.0005

   OUTPUT:

    {:smpl <list[num] | vector[num] | seq[num]>
     :h-mean <num>
     :alpha <num>
     :t-stat <num>
     :dof <num>
     :crtcl-value <num>
     :smpl-mean <num>
     :smpl-std-dev <num>
     :smpl-size <num>}

   DETAILS:

    :smpl = sample data
    :h-mean = hypothesized mean
    :alpha = alpha value
    :t-stat = test statistic
    :dof = degrees of freedom
    :crtcl-value = critical value
    :smpl-mean = sample mean
    :smpl-std-dev =  sample standard deviation
    :smpl-size = sample size

   NOTE:

    Alpha value defaults to 0.05 unless explicitly defined"

  [{sample :smpl h-mean :h-means alpha :alpha :or {alpha 0.05}}]
  (ttest (OneSample. sample h-mean alpha)))


(defn equal-var-ttest

  "Two sample equal variance t-test is a hypothesis test, in which the test statistic follows
   a Student's t-distribution under the null hypothesis. It can be used to determine if
   two sets of data are significantly different from each other. Two sample
   equal variance t-test assumes that the two populations have normal
   distributions and equal variances

   INPUT:

    1) {:smpls <list[list[num]] | vector[vector[num] | seq[seq[num]]> :h-mean <vec[num] | list[num] | seq[num]>}

    2) {:smpls <list[list[num]] | vector[vector[num]] | seq[seq[num]]> :h-mean <vec[num] | list[num] | seq[num]> :alpha <num>}

    Possible alpha values:

    0.40, 0.30, 0.20, 0.15, 0.10, 0.05, 0.025, 0.02, 0.015, 0.01, 0.0075, 0.005, 0.0025, 0.0005

   OUTPUT:

    {:smpls <list[list[num]] | vector[vector[num] | seq[seq[num]]>
     :h-means <list[num] | vector[num] | seq[num]>
     :alpha <num>
     :t-stat <num>
     :dof <num>
     :crtcl-val <num>
     :smpl-means <list[num] | vector[num] | seq[num]>
     :pop-means <list[num] | vector[num] | seq[num]>
     :pool-vars <list[num] | vector[num] | seq[num]>
     :smpl-sizes <list[num] | vector[num] | seq[num]>}

   DETAILS:

    :smpls = samples
    :h-means = hypothesized means
    :alpha = alpha value
    :t-stat test statistic
    :dof = degrees of freedom
    :crtcl-val = critical value
    :smpl-means = sample means
    :pop-means = population means
    :pool-vars = pooled variances
    :smpl-sizes = sample sizes

   NOTE:

    - Alpha value defaults to 0.05 unless explicitly defined

    - Hypothesized means both default to 0 unless explicitly defined"

  [{smpls :smpls h-means :h-means alpha :alpha :or {alpha 0.05 h-means [0 0]}}]
  (ttest (EqualVariance. smpls h-means alpha)))


(defn welch-ttest
  "Two sample unequal variance t-test (welch t-test) is a hypothesis test, in which the test
   statistic follows a Student's t-distribution under the null hypothesis. It can be
   used to determine if two sets of data are significantly different from each
   other. Two sample unequal variance t-test assumes that the two populations
   have normal distributions and unequal variances

   INPUT:

    1) {:smpls <list[list[num]] | vector[vector[num] | seq[seq[num]]>}

    2) {:smpls <list[list[num]] | vector[vector[num]] | seq[seq[num]]> :alpha <num>}

    Possible alpha values:

    0.40, 0.30, 0.20, 0.15, 0.10, 0.05, 0.025, 0.02, 0.015, 0.01, 0.0075, 0.005, 0.0025, 0.0005

   OUTPUT:

    {:smpls <list[list[num]] | vector[vector[num] | seq[seq[num]]>
     :alpha <num>
     :t-stat <num>
     :dof <num>
     :crtcl-val <num>
     :smpl-means <list[num] | vector[num] | seq[num]>
     :smpl-vars <list[num] | vector[num] | seq[num]>
     :smpl-sizes <list[num] | vector[num] | seq[num]>}

   DETAILS:

    :smpls = samples
    :alpha = alpha value
    :t-stat test statistic
    :dof = degrees of freedom
    :crtcl-val = critical value
    :smpl-means = sample means
    :smpl-vars = sample variances
    :smpl-sizes = sample sizes

   NOTE:

    - Alpha value defaults to 0.05 unless explicitly defined"

  [{smpls :smpls alpha :alpha :or {alpha 0.05}}]
  (ttest (Welch. smpls alpha)))

;TODO add extensive docs, specs
(defn rep-msure-ttest [{pops :pops h-means :h-mean alpha :alpha :or {alpha 0.05 h-means [0 0]}}]
  (ttest (RepeatedMeasure. pops h-means alpha)))


