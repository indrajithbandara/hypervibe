(ns hypervibe.core.test
	  (:import [hypervibe.core.api.test OneSample EqualVariance Welch RepeatedMeasure]))
(use 'criterium.core)

(defn osmpl

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

		1) {:smpl <list | vec> :hmean <num>}

		2) {:smpl <list | vec> :hmean <num> :alpha <num>}

		Possible alpha values:

		0.40, 0.30, 0.20, 0.15, 0.10, 0.05, 0.025, 0.02, 0.015, 0.01, 0.0075, 0.005, 0.0025, 0.0005

	   OUTPUT:

		{:smpl <list | vec>
		 :hmean <num>
		 :alpha <num>
		 :t-stat <num>
		 :dof <num>
		 :cval <num>
		 :smean <num>
		 :ssdev <num>
		 :ssize <num>}

	   DETAIL:

		:smpl = sample data
		:hmean = hypothesized mean
		:alpha = alpha value
		:tstat = test statistic
		:dof = degrees of freedom
		:cval = critical value
		:smean = sample mean
		:ssdev =  sample standard deviation
		:ssize = sample size

	   NOTE:

		Alpha value defaults to 0.05 unless explicitly defined

	   SCENARIO:

		You want to show that a new teaching method for pupils struggling to learn English grammar can improve their grammar
		skills to the national average. Your sample would be pupils who received the new teaching method and your
		hypothesized population mean would be the national average score"

	  [{smpl :smpl hmean :hmean alpha :alpha :or {alpha 0.05}}] (OneSample. smpl hmean alpha))


(defn evar

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

		1) {:smpls<list | vec>}

		2) {:smpls <list | vec> :hmeans <list | vec>}

		3) {:smpls <list | vec> :hmeans <list | vec> :alpha <num>}

		Possible alpha values:

		0.40, 0.30, 0.20, 0.15, 0.10, 0.05, 0.025, 0.02, 0.015, 0.01, 0.0075, 0.005, 0.0025, 0.0005

	   OUTPUT:

		{:smpls <list | vec>
		 :hmeans <list | vec>
		 :alpha <num>
		 :tstat <num>
		 :dof <num>
		 :crtcl-val <num>
		 :smeans <list | vec>
		 :pmeans <list | vec>
		 :pool-vars <list | vec>
		 :ssizes <list | vec>}

	   DETAIL:

		:smpls = samples
		:hmeans = hypothesized means
		:alpha = alpha value
		:tstat test statistic
		:dof = degrees of freedom
		:crtcl-val = critical value
		:smeans = sample means
		:pmeans = population means
		:pool-vars = pooled variances
		:ssizes = sample sizes

	   NOTE:

		- Alpha value defaults to 0.05 unless explicitly defined

		- Often the null hypothesis for an independent samples t-test is that the difference between the population means
		  is 0

		- Population means default to 0 unless explicitly defined

	   SCENARIO:

		You want to understand whether first year graduate salaries differ based on gender"

	  [{smpls :smpls hmeans :hmeans alpha :alpha :or {alpha 0.05 hmeans [0 0]}}] (EqualVariance. smpls hmeans alpha))


(defn welch

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

		1) {:smpls <list | vec>}

		2) {:smpls <list | vec>}

		Possible alpha values:

		0.40, 0.30, 0.20, 0.15, 0.10, 0.05, 0.025, 0.02, 0.015, 0.01, 0.0075, 0.005, 0.0025, 0.0005

	   OUTPUT:

		{:smpls <list | vec>
		 :alpha <num>
		 :tstat <num>
		 :dof <num>
		 :crtcl-val <num>
		 :smeans <list | vec>
		 :svars <list | vec>
		 :ssizes <list | vec>}

	   DETAIL:

		:smpls = samples
		:alpha = alpha value
		:tstat = test statistic
		:dof = degrees of freedom
		:crtcl-val = critical value
		:smeans = sample means
		:svars = sample variances
		:ssizes = sample sizes

	   NOTE:

		- Alpha value defaults to 0.05 unless explicitly defined

		SCENARIO:

		Compare the heights in inches of two groups of individuals"

	  [{smpls :smpls alpha :alpha :or {alpha 0.05}}] (Welch. smpls alpha))


(defn rmsure

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

		1) {:smpls <list | vec>}

		2) {:smpls <list | vec> :hmeans <list | vec>}

		3) {:smpls <list | vec> :hmeans <list | vec> :alpha <num>}

	   OUTPUT:

		{:smpls <list | vec>
		 :hmeans <list | vec>
		 :alpha <num>
		 :tstat <num>
		 :dof <num>
		 :crtcl-val <num>
		 :pmeans <list | vec>
		 :std-dev <num>
		 :ssize <num>
		 :diff <num>}

	   DETAIL:

		:smpls = samples
		:hmeans = hypothesized means
		:alpha = alpha value
		:tstat = test statistic
		:dof = degrees of freedom
		:crtcl-val = critical value
		:pmeans = population means
		:std-dev = standard deviation
		:ssize = sample size
		:diff = mean difference

	   NOTE:

		- Alpha value defaults to 0.05 unless explicitly defined

		- Often the null hypothesis for a repeated measure t-test is that the difference between the population means is 0

		- Population means default to 0 unless explicitly defined

	   SCENARIO:

		Measure the performance of participants in a spelling test 'before' and 'after' they underwent a new form of computerised
		teaching method to improve spelling"

	  [{smpls :smpls hmeans :hmeans alpha :alpha :or {alpha 0.05 hmeans [0 0]}}] (RepeatedMeasure. smpls hmeans alpha))


