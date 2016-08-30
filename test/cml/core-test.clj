(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.utils.samples :refer :all]
            [cml.dataset :refer [data-frame]]
            [cml.extract :refer [file-lines]]
            [cml.utils :refer [zip]]
            [cml.core.statistics.estimate :refer [one-sample-conf-inter two-sample-conf-inter]]
            [cml.core.statistics.numerical.test :refer [one-sample-ttest equal-var-ttest welch-ttest rep-measure-ttest]]
            [cml.core.statistics.critical-value :refer [one-tail-cv two-tail-cv]]))

(deftest one-sample-t-test-test
  (is (= (one-sample-ttest {:sample population-one :h-mean 400})
         #cml.statistics.numerical.test.OneSample{:sample [490 500 530 550 580 590 600 600 650 700],
                                                  :h-mean 400,
                                                  :t-statistic 8.700992601418207,
                                                  :dof 9,
                                                  :sample-mean 579.0,
                                                  :sample-standard-deviation 65.05553183413554,
                                                  :sample-size 10})))


(deftest two-sample-t-test-equal-variance
  (is (= (equal-var-ttest {:sample [ballet-dancers football-players] :h-mean [0 0]})
         #cml.statistics.numerical.test.EqualVariance{:sample [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                               [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                                      :h-mean [0 0],
                                                      :t-statistic 1.094722972460392,
                                                      :dof 18,
                                                      :sample-means [87.94999999999999 85.19],
                                                      :population-means [0.0 0.0],
                                                      :pooled-variances [32.382777777777775 31.181000000000015],
                                                      :sample-sizes [10 10]})))


(deftest two-sample-t-test-unequal-variance
  (is (= (welch-ttest {:sample [ballet-dancers football-players]})
         #cml.statistics.numerical.test.Welch{:mean                      (87.94999999999999 85.19),
                                              :sample-variance (32.382777777777775 31.181000000000015),
                                              :size            (10 10),
                                              :t-statistic     1.0947229724603922,
                                              :dof             17.993567997176537})))


(deftest two-sample-repeated-measure-test
  (is (= (rep-measure-ttest {:population [after before] :hp-mean [0 0]})
         #cml.statistics.numerical.test.RepeatedMeasure{:difference-mean                        -11.0,
                                                                  :population-mean    (0.0 0.0),
                                                                  :standard-deviation 13.90443574307614,
                                                                  :size               10,
                                                                  :t-statistic        -2.5017235438103813,
                                                                  :dof                9})))


(deftest one-sample-conf-inter-test
  (is (= (one-sample-conf-inter {:sample population-one :critical-value 1.8331})
         #cml.statistics.estimate.OneSample{:sample-mean                                  579.0,
                                                               :sample-standard-deviation 65.05553183413554,
                                                               :sample-size               10,
                                                               :critical-value            1.8331,
                                                               :upper                     616.7112031961178,
                                                               :lower                     541.2887968038822})))


(deftest two-sample-confidence-interval-test-test
  (is (= (two-sample-conf-inter {:sample [ballet-dancers football-players] :critical-value 2.1009})
         #cml.statistics.estimate.TwoSample{:sample-mean                        (87.94999999999999 85.19),
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


(def dataset "/Users/gra11/IdeaProjects/cml/resources/datasets/adult/adult.data")

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


