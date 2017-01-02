(ns clojure.core-test
  (:require [clojure.test :refer :all]
            [clojure.stats.samples :refer :all]
            [clojure.dataset :refer [data-frame]]
            [clojure.extract :refer [file-lines]]
            [clojure.stats.utils :refer [zip]]
            [clojure.core.stats.test :refer [one-smpl-ttest equal-var-ttest welch-ttest rep-msure-ttest]]
            [clojure.core.stats.estimate.confidence_interval :refer [one-smpl-conf-intvl equal-var-conf-intvl]]))

;TODO change docs to add hmean, null-rej etc
;TODO research more into what constitutes to a rejected null hypothesis
;TODO add alternative hypothesis
;TODO put helper functions into utils name space
;TODO confidence interval for welch and repeated measure ttest
;TODO update docs to add null hypothesis result
;TODO create function that outputs test result
;TODO using math.round will solve problem of dof for welch ttest
;TODO generate chi square distribution using R
;TODO implement Z scores using R
;TODO Implement more tests as per http://www.ats.ucla.edu/stat/mult_pkg/whatstat/ & http://www.ats.ucla.edu/stat/spss/whatstat/whatstat.htm
;TODO create protocol named Conduct which compares the absolute value of the t statistic with the critical value ang outputs the test result
;TODO reference here for different distrubutions http://www.itl.nist.gov/div898/handbook/eda/section3/eda367.htm
;TODO Levene's Test for Equality of Variances
;TODO start implementing chart api

(deftest one-sample-t-test-test
  (is (= (one-smpl-ttest {:smpl population-one :hmean 400})
         {:dof 9,
          :type :OneSampleTTest,
          :crtcl-val 1.83311293265624,
          :hmean 400,
          :smpl-size 10,
          :smpl-mean 579.0,
          :rej-null? true,
          :alpha 0.05,
          :smpl-std-dev 65.05553183413554,
          :tstat 8.700992601418207,
          :smpl [490 500 530 550 580 590 600 600 650 700],
          :diff 6.867879668761967})))


(deftest two-sample-t-test-equal-variance
  (is (= (equal-var-ttest {:smpls [ballet-dancers football-players]})
         {:pop-means [0.0 0.0],
          :tstat 1.094722972460392,
          :dof 18,
          :smpl-means [87.94999999999999 85.19],
          :hmeans [0 0],
          :smpl-sizes [10 10],
          :crtcl-val 1.73406360661754,
          :rej-null? false,
          :alpha 0.05,
          :pool-vars [32.382777777777775 31.181000000000015],
          :smpls [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3] [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
          :diff -0.6393406341571481})))


(deftest two-sample-t-test-unequal-variance-welch
  (is (= (welch-ttest {:smpls [ballet-dancers football-players]})
         {:tstat 1.0947229724603922,
          :dof 17.993567997176537,
          :type clojure.stats.test.Welch,
          :smpl-means [87.94999999999999 85.19],
          :smpl-sizes [10 10],
          :crtcl-val 1.73406360661754,
          :smpl-vars [32.382777777777775 31.181000000000015],
          :rej-null? false,
          :alpha 0.05,
          :smpls [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3] [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
          :diff -0.6393406341571479})))



(deftest two-sample-repeated-measure-test
  (is (= (rep-msure-ttest {:smpls [after before]})
         {:pop-means [0.0 0.0],
          :tstat -2.5017235438103813,
          :dof 9,
          :diff-mean -11.0,
          :type clojure.stats.test.RepeatedMeasure,
          :hmeans [0 0],
          :crtcl-val 1.83311293265624,
          :rej-null? true,
          :smpl-size 10,
          :alpha 0.05,
          :std-dev 13.90443574307614,
          :smpls [[200 210 210 170 220 180 190 190 220 210] [220 240 225 180 210 190 195 200 210 240]]})))


(deftest one-sample-conf-inter-test
  (is (= (one-smpl-conf-intvl {:smpl population-one :crtcl-val  1.83311293265624 :hmean 400})
         {:upper 216.71146925144888,
          :type :ConfidenceInterval,
          :crtcl-val 1.83311293265624,
          :hmean 400,
          :smpl-size 10,
          :smpl-mean 579.0,
          :mean-diff 179.0,
          :lower 141.28853074855112,
          :smpl-std-dev 65.05553183413554,
          :smpl [490 500 530 550 580 590 600 600 650 700]})))


(deftest two-sample-confidence-interval-test-test
  (is (= (equal-var-conf-intvl {:smpls [ballet-dancers football-players] :crtcl-val 2.1009})
         {:smpls [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3] [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
          :smpl-vars [32.382777777777775 31.181000000000015],
          :smpl-means [87.94999999999999 85.19],
          :smpl-sizes [10 85.19],
          :crtcl-val 2.1009,
          :upper 8.05675922207777,
          :lower -2.536759222077789
          :type :ConfidenceInterval})))


(deftest compose-one-sample-ttest-confidence-interval
  (is (= ((comp one-smpl-conf-intvl) (one-smpl-ttest {:smpl population-one :hmean 400}))
         {:upper 216.71146925144888,
          :type :ConfidenceInterval,
          :crtcl-val 1.83311293265624,
          :hmean 400,
          :smpl-size 10,
          :smpl-mean 579.0,
          :mean-diff 179.0,
          :lower 141.28853074855112,
          :smpl-std-dev 65.05553183413554,
          :smpl [490 500 530 550 580 590 600 600 650 700]})))

#_(def dataset "/Users/adebesing/Dropbox/Workspace/clojure-stats/resources/datasets/adult/adult.data")


#_(data-frame {:column-names [:age :department :salary
                            :degree :study-time :marital-status
                            :job :family-status :race
                            :gender :n1 :n2 :n3 :country :salary-range]
             :delimiter    ","
             :file-path    dataset
             :type         :csv/read
             :return '()})

#_(data-frame {:column-names [:age :department :salary
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
;If the absolute value of your test statistic is greater than the critical value, you can declare statistical significance and reject the null hypothesis

#_(def ostest (one-smpl-ttest {:sample [1 2 3] :h-mean 4 :tail :one})) ;Polymorphic on the ttest interface
#_(def evtest (equal-var-ttest {:samples [[1 2 3] [4 5 6]] :h-mean [0 0] :tail :one}))
;(ttest ostest)
;(ttest evtest)