(ns clojure.core.stats.test
  (:require [clojure.stats.test :refer [ttest hypothesis-test]])
  (:import [clojure.stats.test OneSample EqualVariance Welch RepeatedMeasure OneSampleHypothesisTest]))
(use 'criterium.core)


(defn one-smpl-hypo-test
  [{smpl :smpl h-mean :h-mean alpha :alpha reject? :reject?}]
  (hypothesis-test (OneSampleHypothesisTest. smpl h-mean alpha reject?)))


(defn one-smpl-ttest

  "OVERVIEW:

    One sample t-test is a hypothesis test, in which the test statistic follows a student's t-distribution
    under the null hypothesis. It can be used to determine if two sets of data are significantly
    different from each other. The purpose of this test is to determine whether the mean
    of the population from which the sample was drawn and a hypothesized population
    mean are the same

    The one-sample t-test is used to determine whether a sample comes from a population with a specific mean. This
    population mean is not always known, but is sometimes hypothesized

   ASSUMPTION:

    - Variables are measured at interval level
    - Samples are independent
    - Random sample
    - No significant outliers
    - Variables are normally distributed | approximately normally distributed

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

   DETAIL:

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

    Alpha value defaults to 0.05 unless explicitly defined

   SCENARIO:

    You want to show that a new teaching method for pupils struggling to learn English grammar can improve their grammar
    skills to the national average. Your sample would be pupils who received the new teaching method and your
    hypothesized population mean would be the national average score"

  [{smpl :smpl h-mean :h-mean alpha :alpha :or {alpha 0.05}}]
  (ttest (OneSample. smpl h-mean alpha)))


(defn equal-var-ttest

  "OVERVIEW:

    Two sample equal variance t-test is a hypothesis test, in which the test statistic follows a student's t-distribution under
    the null hypothesis. It can be used to determine if two sets of data are significantly different from each other. The
    purpose of this test is to determine whether the means of the populations from which the samples were drawn
    are the same

    The two sample equal variance ttest compares the means of two samples in order to test whether the associated population means
    are significantly different

   ASSUMPTION:

    - Variables are measured at interval level
    - Samples are independent
    - Random sample
    - No significant outliers
    - Variables are normally distributed | approximately normally distributed
    - Homogeneity of variance

   INPUT:

    1) {:smpls <list[list[num]] | vector[vector[num] | seq[seq[num]]>}

    2) {:smpls <list[list[num]] | vector[vector[num]] | seq[seq[num]]> :h-means <vec[num] | list[num] | seq[num]>}

    3) {:smpls <list[list[num]] | vector[vector[num]] | seq[seq[num]]> :h-means <vec[num] | list[num] | seq[num]> :alpha <num>}

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

   DETAIL:

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

    - Often the null hypothesis for an independent samples t-test is that the difference between the population means
      is 0

    - Population means default to 0 unless explicitly defined

   SCENARIO:

    You want to understand whether first year graduate salaries differ based on gender"

  [{smpls :smpls h-means :h-means alpha :alpha :or {alpha 0.05 h-means [0 0]}}]
  (ttest (EqualVariance. smpls h-means alpha)))


(defn welch-ttest

  "OVERVIEW:

    Welch's two sample unequal variance t-test is a hypothesis test, in which the test statistic follows a
    student's t-distribution under the null hypothesis. It can be used to determine if two sets of data
    are significantly different from each other. The purpose of this test is to determine whether the
    means of the populations from which the samples were drawn are the same

    Welch's two sample unequal variance ttest compares the means of two samples in order to test whether the
    associated population means are significantly different

   ASSUMPTION:

   - Variables are measured at interval level
   - Samples are independent
   - Random sample
   - No significant outliers
   - Variables are normally distributed | approximately normally distributed
   - Nonhomogeneity of variance

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

   DETAIL:

    :smpls = samples
    :alpha = alpha value
    :t-stat = test statistic
    :dof = degrees of freedom
    :crtcl-val = critical value
    :smpl-means = sample means
    :smpl-vars = sample variances
    :smpl-sizes = sample sizes

   NOTE:

    - Alpha value defaults to 0.05 unless explicitly defined

    SCENARIO:

    Compare the heights in inches of two groups of individuals"

  [{smpls :smpls alpha :alpha :or {alpha 0.05}}]
  (ttest (Welch. smpls alpha)))


(defn rep-msure-ttest

  "OVERVIEW:

    Two repeated measure t-test is a hypothesis test, in which the test statistic follows a student's t-distribution
    under the null hypothesis. The repeated measure t-test compares the means of two related groups to determine
    whether there is a statistically significant difference between these means

    The repeated measure t-test looks for differences between means when participants are measured on the same dependent
    variable under two different conditions

   ASSUMPTION:

    - Variables are measured at interval level
    - Samples are independent
    - Random sample
    - No significant outliers
    - Variables are normally distributed | approximately normally distributed
    - Homogeneity of variance
    - Participants tested more than once
    - Same participants tested on each occasion

   INPUT:

    1) {:smpls <list[list[num]] | vector[vector[num] | seq[seq[num]]>}

    2) {:smpls <list[list[num]] | vector[vector[num] | seq[seq[num]]> :h-means <vec[num] | list[num] | seq[num]>}

    3) {:smpls <list[list[num]] | vector[vector[num] | seq[seq[num]]> :h-means <vec[num] | list[num] | seq[num]> :alpha <num>}

   OUTPUT:

    {:smpls <list[list[num]] | vector[vector[num] | seq[seq[num]]>
     :h-means <list[num] | vector[num] | seq[num]>
     :alpha <num>
     :t-stat <num>
     :dof <num>
     :crtcl-val <num>
     :pop-means <list[num] | vector[num] | seq[num]>
     :std-dev <num>
     :smpl-size <num>
     :diff-mean <num>}

   DETAIL:

    :smpls = samples
    :h-means = hypothesized means
    :alpha = alpha value
    :t-stat = test statistic
    :dof = degrees of freedom
    :crtcl-val = critical value
    :pop-means = population means
    :std-dev = standard deviation
    :smpl-size = sample size
    :diff-mean = mean difference

   NOTE:

    - Alpha value defaults to 0.05 unless explicitly defined

    - Often the null hypothesis for a repeated measure t-test is that the difference between the population means is 0

    - Population means default to 0 unless explicitly defined

   SCENARIO:

    Measure the performance of participants in a spelling test 'before' and 'after' they underwent a new form of computerised
    teaching method to improve spelling"

  [{smpls :smpls h-means :h-means alpha :alpha :or {alpha 0.05 h-means [0 0]}}]
  (ttest (RepeatedMeasure. smpls h-means alpha)))


