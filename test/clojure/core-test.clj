(ns clojure.core-test
  (:require [clojure.test :refer :all]
            [clojure.utils.samples :refer :all]
            [clojure.dataset :refer [data-frame]]
            [clojure.extract :refer [file-lines]]
            [clojure.utils :refer [zip]]
            [clojure.core.stats.estimate :refer [one-sample-conf-inter two-sample-conf-inter]]
            [clojure.core.stats.lom.interval.test :refer [one-sample-ttest equal-var-ttest welch-ttest rep-measure-ttest]]
            [clojure.core.stats.critical-value :refer [one-tail-cv two-tail-cv]]))

;TODO reformat project structure as per http://www.abs.gov.au/websitedbs/a3121120.nsf/home/statistical+language+-+what+are+variables
;TODO Implement more tests as per http://www.ats.ucla.edu/stat/mult_pkg/whatstat/ & http://www.ats.ucla.edu/stat/spss/whatstat/whatstat.htm
;TODO rename all cs to cs

(deftest one-sample-t-test-test
  (is (= (one-sample-ttest {:sample population-one :h-mean 400})
         #clojure.stats.lom.interval.test.OneSample{:sample [490 500 530 550 580 590 600 600 650 700],
                                                     :h-mean 400,
                                                     :t-statistic 8.700992601418207,
                                                     :dof 9,
                                                     :sample-mean 579.0,
                                                     :sample-standard-deviation 65.05553183413554,
                                                     :sample-size 10})))


(deftest two-sample-t-test-equal-variance
  (is (= (equal-var-ttest {:sample [ballet-dancers football-players] :h-mean [0 0]})
         #clojure.stats.lom.interval.test.EqualVariance{:sample [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                               [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                                      :h-mean [0 0],
                                                      :t-statistic 1.094722972460392,
                                                      :dof 18,
                                                      :sample-means [87.94999999999999 85.19],
                                                      :population-means [0.0 0.0],
                                                      :pooled-variances [32.382777777777775 31.181000000000015],
                                                      :sample-sizes [10 10]})))


(deftest two-sample-t-test-unequal-variance-welch
  (is (= (welch-ttest {:sample [ballet-dancers football-players]})
         #clojure.stats.lom.interval.test.Welch{:sample [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                       [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                                 :t-statistic 1.0947229724603922,
                                                 :dof 17.993567997176537,
                                                 :sample-means [87.94999999999999 85.19],
                                                 :sample-variances [32.382777777777775 31.181000000000015],
                                                 :sample-sizes [10 10]})))


;fix
(deftest two-sample-repeated-measure-test
  (is (= (rep-measure-ttest {:population [after before] :h-mean [0 0]})
         #clojure.stats.lom.interval.test.RepeatedMeasure{:population [[200 210 210 170 220 180 190 190 220 210]
                                                                     [220 240 225 180 210 190 195 200 210 240]],
                                                        :h-mean [0 0],
                                                        :t-statistic -2.5017235438103813,
                                                        :dof 9,
                                                        :population-means [0.0 0.0],
                                                        :standard-deviation 13.90443574307614,
                                                        :population-size 10,
                                                        :difference-mean -11.0})))

;fix
(deftest one-sample-conf-inter-test
  (is (= (one-sample-conf-inter {:sample population-one :critical-value 1.8331})
         #clojure.stats.estimate.OneSample{:sample-mean               579.0,
                                            :sample-standard-deviation 65.05553183413554,
                                            :sample-size               10,
                                            :critical-value            1.8331,
                                            :upper                     616.7112031961178,
                                            :lower                     541.2887968038822})))


(deftest two-sample-confidence-interval-test-test
  (is (= (two-sample-conf-inter {:sample [ballet-dancers football-players] :critical-value 2.1009})
         #clojure.stats.estimate.TwoSample{:sample-mean     (87.94999999999999 85.19),
                                            :sample-variance (32.382777777777775 31.181000000000015),
                                            :sample-size     (10 10),
                                            :critical-value  2.1009,
                                            :upper           8.05675922207777,
                                            :lower           -2.536759222077789})))


(deftest two-tail-significance-test-test
  (is (= (two-tail-cv {:dof 9 :alpha 0.05})
         {:critical-value 2.2621, :dof 9, :alpha 0.05})))


(deftest one-tail-significance-test-test
  (is (= (one-tail-cv {:dof 9 :alpha 0.05})
         {:critical-value 1.8331, :dof 9, :alpha 0.05})))


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

;http://www.ats.ucla.edu/stat/mult_pkg/whatstat/choosestat.html
;http://www.ats.ucla.edu/stat/stata/whatstat/whatstat.htm
;http://www.ats.ucla.edu/stat/mult_pkg/whatstat/nominal_ordinal_interval.htm

(def ostest (one-sample-ttest {:sample [1 2 3] :h-mean 4})) ;Polymorphic on the ttest interface
(def evtest (equal-var-ttest {:sample [[1 2 3] [4 5 6]] :h-mean [0 0]}))
;(ttest ostest)
;(ttest evtest)