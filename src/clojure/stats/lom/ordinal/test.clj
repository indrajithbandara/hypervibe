(ns clojure.stats.lom.ordinal.test)

(defprotocol Ordinal
  (ttest [this] "Conducts a ttest on a dataset. A t-test looks at the t-statistic, the t-distribution and
                 degrees of freedom to determine the probability of difference between populations"))

(defrecord OneSampleMedian []
  Ordinal
  (ttest [type]
    (println type)))