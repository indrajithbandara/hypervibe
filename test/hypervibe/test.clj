(ns hypervibe.test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as m]
            [hypervibe.core.api.samples :refer :all]
            [hypervibe.core.api.test :refer [disc tstat]]
            [hypervibe.core.test :refer [osmpl evar welch rmsure]]
            [hypervibe.core.confidence_interval :refer [osc-intvl evc-intvl]]
            [criterium.core :as cri]))
(m/set-current-implementation :vectorz)

;TODO add benchmarks tests and compare claypool to pmap
;TODO rename ttest to prestat or crunch
;TODO rename tstat to ttest as it is computing the test statistic
;TODO update docs
;TODO make all functions use mikera vector and all accompanying api
;TODO add critical value and critical value diff and pvalue
;TODO confidence intervals for each test
;TODO add specs to high level api
;TODO Implement function for looking up Pvalue
;TODO use zac tellman primitive math lib
;TODO change to mikera.vectors


(deftest one-sample-t-test-test
  (is (= (disc (osmpl {:smpl (m/array [490 500 530 550 580 590 600 600 650 700]) :hmean 400}))
         #hypervibe.core.api.test.OneSample{:smpl  #vectorz/vector[490.0 500.0 530.0 550.0 580.0 590.0 600.0 600.0 650.0 700.0],
                                            :hmean 400,
                                            :alpha 0.05,
                                            :smean 579.0,
                                            :ssdev 65.05553183413554,
                                            :ssize 10,
                                            :dof   9,
                                            :diff  nil})))

(deftest one-sample-t-test-test-tstat
  (is (= (tstat (disc (osmpl {:smpl (m/array [490 500 530 550 580 590 600 600 650 700]) :hmean 400})))
         #hypervibe.core.api.test.OneSample{:smpl  #vectorz/vector[490.0 500.0 530.0 550.0 580.0 590.0 600.0 600.0 650.0 700.0],
                                            :hmean 400,
                                            :alpha 0.05,
                                            :smean 579.0,
                                            :ssdev 65.05553183413554,
                                            :ssize 10,
                                            :tstat 8.700992601418207,
                                            :dof   9,
                                            :diff  nil})))

(deftest two-sample-t-test-evariance
  (is (= (disc (evar {:smpls [ballet-dancers football-players]}))
         #hypervibe.core.api.test.EqualVariance{:smpls  [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                         [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                                :hmeans [0 0],
                                                :alpha  0.05,
                                                :smeans [87.94999999999999 85.19],
                                                :pmeans [0.0 0.0],
                                                :pvars  [32.382777777777775 31.181000000000015],
                                                :ssizes [10 10],
                                                :dof    18
                                                :diff   nil})))

(deftest two-sample-t-test-evariance-tstat
  (is (= (tstat (disc (evar {:smpls [ballet-dancers football-players]})))
         #hypervibe.core.api.test.EqualVariance{:smpls  [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                         [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                                :hmeans [0 0],
                                                :alpha  0.05,
                                                :smeans [87.94999999999999 85.19],
                                                :pmeans [0.0 0.0],
                                                :pvars  [32.382777777777775 31.181000000000015],
                                                :ssizes [10 10],
                                                :dof    18,
                                                :tstat  1.094722972460392
                                                :diff nil})))

(deftest two-sample-t-test-unequal-variance-welch
  (is (= (disc (welch {:smpls [ballet-dancers football-players]}))
         #hypervibe.core.api.test.Welch{:smpls  [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                 [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                        :alpha  0.05,
                                        :smeans [87.94999999999999 85.19],
                                        :svars  [32.382777777777775 31.181000000000015],
                                        :ssizes [10 10],
                                        :dof    17.993567997176537})))

(deftest two-sample-t-test-unequal-variance-welch-tstat
  (is (= (tstat (disc (welch {:smpls [ballet-dancers football-players]})))
         #hypervibe.core.api.test.Welch{:smpls  [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                                                 [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
                                        :alpha  0.05,
                                        :smeans [87.94999999999999 85.19],
                                        :svars  [32.382777777777775 31.181000000000015],
                                        :ssizes [10 10],
                                        :tstat  1.0947229724603922,
                                        :dof    17.993567997176537})))



(deftest two-sample-repeated-measure-test
  (is (= (disc (rmsure {:smpls [after before]}))
         #hypervibe.core.api.test.RepeatedMeasure{:smpls  [[200 210 210 170 220 180 190 190 220 210]
                                                           [220 240 225 180 210 190 195 200 210 240]],
                                                  :hmeans [0 0],
                                                  :alpha  0.05,
                                                  :means  [0.0 0.0],
                                                  :dmean  -11.0,
                                                  :sdev   13.904435743076139,
                                                  :ssize  10,
                                                  :dof    9})))

(deftest two-sample-repeated-measure-test-tstat
  (is (= (tstat (disc (rmsure {:smpls [after before]})))
         #hypervibe.core.api.test.RepeatedMeasure{:smpls  [[200 210 210 170 220 180 190 190 220 210]
                                                           [220 240 225 180 210 190 195 200 210 240]],
                                                  :hmeans [0 0],
                                                  :alpha  0.05,
                                                  :means  [0.0 0.0],
                                                  :dmean  -11.0,
                                                  :sdev   13.904435743076139,
                                                  :ssize  10,
                                                  :dof    9,
                                                  :tstat  -2.5017235438103813})))


(deftest one-sample-conf-inter-test
  (is (= (osc-intvl {:smpl population-one :cval 1.83311293265624 :hmean 400})
         {:upper 216.71146925144888,
          :ssdev 65.05553183413554,
          :mdiff 179.0,
          :cval  1.83311293265624,
          :ssize 10,
          :smean 579.0,
          :lower 141.28853074855112,
          :hmean 400,
          :smpl  #vectorz/vector[490.0 500.0 530.0 550.0 580.0 590.0 600.0 600.0 650.0 700.0]})))


(deftest two-sample-confidence-interval-test-test
  (is (= (evc-intvl {:smpls [ballet-dancers football-players] :cval 2.1009})
         {:smpls  [[89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3]
                   [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9]],
          :svars  [32.382777777777775 31.181000000000015],
          :smeans [87.94999999999999 85.19],
          :ssizes [10 85.19],
          :cval   2.1009,
          :upper  8.05675922207777,
          :lower  -2.536759222077789})))


#_(deftest compose-one-sample-ttest-confidence-interval
  (is (= ((comp osc-intvl) (tstat (ttest (osmpl {:smpl population-one :hmean 400}))))
         {:upper 216.71140891977285,
          :ssdev 65.05553183413554,
          :mdiff 179.0,
          :cval  1.83311,
          :ssize 10,
          :smean 579.0,
          :lower 141.28859108022715,
          :hmean 400,
          :smpl  #vectorz/vector[490.0 500.0 530.0 550.0 580.0 590.0 600.0 600.0 650.0 700.0]})))

(def dummy1 (range 1000000))

;(cri/with-progress-reporting
;  (cri/bench
;    (ttest (osmpl {:smpl dummy1 :hmean 400}))))

;(ttest (osmpl {:smpl dummy1 :hmean 400}))

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