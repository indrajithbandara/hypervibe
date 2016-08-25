(ns cml.core-test
  (:require [clojure.test :refer :all]
            [cml.utils.samples :refer :all]
            [cml.dataset :refer [data-frame]]
            [cml.extract :refer [file-lines]]
            [cml.utils :refer [zip]]
            [cml.core.statistics.estimate :refer [one-sample-conf-inter two-sample-conf-inter]]
            [cml.core.statistics.test :refer [one-sample-ttest equal-var-ttest welch-ttest rep-measure-ttest]]
            [cml.core.statistics.critical-value :refer [one-tail-cv two-tail-cv]]
            [clojure.core.matrix.operators :as op]
            [clojure.core.matrix :as matrix]
            [clojure.walk :as walk]))
(use 'com.rpl.specter)

(deftest one-sample-t-test-test
  (is (= (one-sample-ttest {:sample population-one :h-mean 400})
         #cml.statistics.test.OneSample{:sample-mean               579.0,
                                        :sample-standard-deviation 65.05553183413554,
                                        :sample-hypothetical-mean  400,
                                        :sample-size               10,
                                        :t-statistic               8.700992601418207,
                                        :dof                       9})))


(deftest two-sample-t-test-equal-variance
  (is (= (equal-var-ttest {:sample [ballet-dancers football-players] :hp-mean [0 0]})
         #cml.statistics.test.EqualVariance{:mean                                (87.94999999999999 85.19),
                                                                :population-mean (0.0 0.0),
                                                                :pooled-variance (32.382777777777775 31.181000000000015),
                                                                :size            (10 10),
                                                                :t-statistic     1.094722972460392,
                                                                :dof             18})))

(deftest two-sample-t-test-unequal-variance
  (is (= (welch-ttest {:sample [ballet-dancers football-players]})
         #cml.statistics.test.Welch{:mean                      (87.94999999999999 85.19),
                                              :sample-variance (32.382777777777775 31.181000000000015),
                                              :size            (10 10),
                                              :t-statistic     1.0947229724603922,
                                              :dof             17.993567997176537})))


(deftest two-sample-repeated-measure-test
  (is (= (rep-measure-ttest {:population [after before] :hp-mean [0 0]})
         #cml.statistics.test.RepeatedMeasure{:difference-mean                        -11.0,
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


;WORKSPACE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dataset "/Users/gregadebesin/IdeaProjects/cml/resources/datasets/adult/adult.data")

(data-frame {:column-names [:age :department :salary
                            :degree :study-time :marital-status
                            :job :family-status :race
                            :gender :n1 :n2 :n3 :country :salary-range]
             :delimiter    ","
             :file-path    dataset
             :type         :csv/read
             :return '()})

; 0.121288
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


;(pvalues ((comp one-sample-conf-inter) (data {:sample (range 1 1000000) :critical-value 2 :spec :one-sample-conf-inter})))

;;Data is specd not functions
;((comp one-sample-conf-inter) (data {:sample (range 1 1000000) :critical-value 2 :spec :one-sample-conf-inter}))



#_(defn two-sample-conf-inter2 [{:keys [sample critical-value]}]
    (confidence-interval (TwoSample. @(future (map mean sample))
                                     @(future (map #(:variance (variance (Sample. (mean %) %))) sample))
                                     @(future (map count sample)) critical-value)))
;http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/stat/inference/ChiSquareTest.html


(def observed [[60 300] [10 390]])
(def total (double (matrix/esum observed)))
(def row-total (map #(reduce + %) observed))
(def column-total (map #(reduce + %) (matrix/columns observed)))

(def expected-val '(33.1578947368421 326.8421052631579 36.8421052631579 363.1578947368421))

(def expected
  (partition 2
             (for [rt (map #(reduce + %) observed)
                   ct (map #(reduce + %) (matrix/columns observed))]
               (/ (* rt ct) (double (matrix/esum observed))))))

(matrix/esum (matrix/emap op// (matrix/emap (fn [x] (op/* x x)) (matrix/sub observed expected))
                          expected))


;TODO seperate out observed vs expected values and use below (values) of (fn [observed expected] ...)
(defn chi-square
  "Assumes data to be in the form
  [[x1 observed, x1 expected] [x2 observed, x2 expected]].
   The Chi-square test computes the sum of the squares of the differences in values"
  [values]
  (reduce + 0
          (map (fn [[observed expected]]
                 (double (/ (matrix/pow (- observed expected) 2) expected))) values)))

(let [observed (atom [2 3 4 5])]
  (walk/walk (fn [vecs] (map (fn [nums] (do (* (first @observed) nums) (swap! observed next))) vecs)) vec [[1 2] [3 34]]))

(let [exp (atom '(0 33.1578947368421 326.8421052631579 36.8421052631579 363.1578947368421))]
  (matrix/emap (fn [x] (do (swap! exp pop) (* x (first @exp)))) observed))


(matrix/esum
  (let [exp (atom '(0 33.1578947368421 326.8421052631579 36.8421052631579 363.1578947368421))]
    (matrix/emap
      (fn [nums]
        (do (swap! exp pop) (/ (* (- nums (first @exp)) (- nums (first @exp))) (first @exp)))) observed)))


(defn chi-square [observed]
  (let [expected (atom (conj (for [row-total (map #(reduce + %) observed)
                                   column-total (map #(reduce + %) (matrix/columns observed))]
                               (/ (* row-total column-total)
                                  (double (matrix/esum observed)))) :sentinal))]
    (matrix/esum (matrix/emap (fn [nums]
                                (do (swap! expected next)
                                    (/ (* (- nums (first @expected))
                                          (- nums (first @expected)))
                                       (first @expected)))) observed))))


