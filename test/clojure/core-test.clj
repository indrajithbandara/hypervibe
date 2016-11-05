(ns clojure.core-test
  (:require [clojure.test :refer :all]
            [clojure.stats.utils.samples :refer :all]
            [clojure.dataset :refer [data-frame]]
            [clojure.extract :refer [file-lines]]
            [clojure.utils :refer [zip]]
            [clojure.core.stats.estimate :refer [one-smpl-conf-inter two-smpl-conf-inter]]
            [clojure.core.stats.lom.interval.test :refer [one-smpl-ttest equal-var-ttest welch-ttest rep-msure-ttest]]
            [clojure.stats.distribution.t.critical-value :refer [critical-value]]))


;TODO move all tests to new t table
;TODO start documentation!
;TODO using math.round will solve problem of dof for welch ttest
;TODO generate chi square distribution using R
;TODO implement Z scores using R
;TODO Implement more tests as per http://www.ats.ucla.edu/stat/mult_pkg/whatstat/ & http://www.ats.ucla.edu/stat/spss/whatstat/whatstat.htm
;TODO create protocol named Conduct which compares the absolute value of the t statistic with the critical value ang outputs the test result
;TODO reference here for different distrubutions http://www.itl.nist.gov/div898/handbook/eda/section3/eda367.htm

(deftest one-sample-t-test-test
  (is (= (one-smpl-ttest {:sample population-one :h-mean 400})
         #clojure.stats.lom.interval.test.OneSample{:sample [490 500 530 550 580 590 600 600 650 700],
                                                    :h-mean 400,
                                                    :alpha 0.05,
                                                    :t-statistic 8.700992601418207,
                                                    :dof 9,
                                                    :critical-value 1.83311293265624,
                                                    :sample-mean 579.0,
                                                    :sample-standard-deviation 65.05553183413554,
                                                    :sample-size 10})))


(deftest two-sample-t-test-equal-variance
  (is (= (equal-var-ttest {:samples [ballet-dancers football-players] :h-mean [0 0] :tail :one})
         #clojure.stats.lom.interval.test.EqualVariance{:samples [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                                  [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                                        :h-mean [0 0],
                                                        :alpha 0.05,
                                                        :tail :one,
                                                        :t-statistic 1.094722972460392,
                                                        :dof 18,
                                                        :critical-value 1.7341,
                                                        :sample-means [87.94999999999999 85.19],
                                                        :population-means [0.0 0.0],
                                                        :pooled-variances [32.382777777777775 31.181000000000015],
                                                        :sample-sizes [10 10]})))


(deftest two-sample-t-test-unequal-variance-welch
  (is (= (welch-ttest {:samples [ballet-dancers football-players]})
         #clojure.stats.lom.interval.test.Welch{:samples [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                       [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                                 :t-statistic 1.0947229724603922,
                                                 :dof 17.993567997176537,
                                                 :alpha 0.05
                                                 :sample-means [87.94999999999999 85.19],
                                                 :sample-variances [32.382777777777775 31.181000000000015],
                                                 :sample-sizes [10 10]})))



(deftest two-sample-repeated-measure-test
  (is (= (rep-msure-ttest {:population [after before] :h-mean [0 0] :tail :one})
         #clojure.stats.lom.interval.test.RepeatedMeasure{:population [[200 210 210 170 220 180 190 190 220 210]
                                                                       [220 240 225 180 210 190 195 200 210 240]],
                                                          :h-mean [0 0],
                                                          :alpha 0.05,
                                                          :tail :one,
                                                          :t-statistic -2.5017235438103813,
                                                          :dof 9,
                                                          :critical-value 1.8331,
                                                          :population-means [0.0 0.0],
                                                          :standard-deviation 13.90443574307614,
                                                          :population-size 10,
                                                          :difference-mean -11.0})))


(deftest one-sample-conf-inter-test
  (is (= (one-smpl-conf-inter {:sample population-one :critical-value 1.8331})
         #clojure.stats.estimate.OneSample{:sample [490 500 530 550 580 590 600 600 650 700],
                                           :critical-value 1.8331,
                                           :sample-standard-deviation 65.05553183413554,
                                           :sample-mean 579.0,
                                           :sample-size 10,
                                           :upper 616.7112031961178,
                                           :lower 541.2887968038822})))


(deftest two-sample-confidence-interval-test-test
  (is (= (two-smpl-conf-inter {:sample [ballet-dancers football-players] :critical-value 2.1009})
         #clojure.stats.estimate.TwoSample{:samples [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                     [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                           :critical-value 2.1009,
                                           :sample-variances [32.382777777777775 31.181000000000015],
                                           :sample-means [87.94999999999999 85.19],
                                           :sample-sizes [10 85.19],
                                           :upper 8.05675922207777,
                                           :lower -2.536759222077789})))


(deftest two-tail-significance-test-test
  (is (= (critical-value {:dof 9 :alpha 0.05 :tail :one}) 1.8331)))


(deftest one-tail-significance-test-test
  (is (= (critical-value {:dof 9 :alpha 0.05 :tail :two}) 2.2621)))


(def dataset "/Users/gregadebesin/Dropbox/Workspace/clojure-stats/resources/datasets/adult/adult.data")


(data-frame {:column-names [:age :department :salary
                            :degree :study-time :marital-status
                            :job :family-status :race
                            :gender :n1 :n2 :n3 :country :salary-range]
             :delimiter    ","
             :file-path    dataset
             :type         :csv/read
             :return '()})

(data-frame {:column-names [:age :department :salary
                            :degree :study-time :marital-status
                            :job :family-status :race
                            :gender :n1 :n2 :n3 :country :salary-range]
             :delimiter    ","
             :file-path    dataset
             :type         :csv/read
             :xform        (comp clojure.string/upper-case
                                 #(clojure.string/replace % #" " ""))
             :return       []})

;NOTES
;http://www.ats.ucla.edu/stat/mult_pkg/whatstat/choosestat.html
;http://www.ats.ucla.edu/stat/stata/whatstat/whatstat.htm
;http://www.ats.ucla.edu/stat/mult_pkg/whatstat/nominal_ordinal_interval.htm
;If alpha equals 0.05, then your confidence level is 0.95. If you increase alpha, you both increase the probability of incorrectly rejecting the null hypothesis and also decrease your confidence level.
;A two-tailed test will test both if the mean is significantly greater than x and if the mean significantly less than x
;LOM = level of measurement

#_(def ostest (one-smpl-ttest {:sample [1 2 3] :h-mean 4 :tail :one})) ;Polymorphic on the ttest interface
#_(def evtest (equal-var-ttest {:samples [[1 2 3] [4 5 6]] :h-mean [0 0] :tail :one}))
;(ttest ostest)
;(ttest evtest)