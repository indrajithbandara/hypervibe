(ns cml.core.statistics.test
  (:require [cml.statistics.test :refer [t-test pearson-chi-square]]
            [cml.utils.variation :refer [standard-deviation variance]]
            [cml.utils.central-tendancy :refer [mean difference]]
            [clojure.core.matrix.operators :as op])
  (:import [cml.statistics.test OneSample EqualVariance Welch RepeatedMeasure Independance]
           [cml.utils.variation Sample Pooled]))
(use 'clojure.core.matrix)


(defn one-sample-ttest [{:keys [sample h-mean]}]
  (let [mean ^double (mean sample)]
    (t-test (OneSample. mean (:standard-deviation (standard-deviation (Sample. mean sample)))
                        h-mean
                        (count sample)))))


(defn equal-var-ttest [{:keys [sample hp-mean]}]
  (t-test (EqualVariance. (map mean sample)
                          (map mean (partition 1 hp-mean))
                          (map #(:variance (variance (Pooled. (mean %) % (- (count %) 1)))) sample)
                          (map count sample))))


(defn welch-ttest [{:keys [sample]}]
  (t-test (Welch. (map mean sample)
                  (map #(:variance (variance (Sample. (mean %) %))) sample)
                  (map count sample))))


(defn rep-measure-ttest [{:keys [population hp-mean]}]
  (let [[population-one population-two] population
        population-mean-difference ^double (mean (difference population))]
    (t-test (RepeatedMeasure. population-mean-difference
                              (map mean (partition 1 hp-mean))
                              (:standard-deviation (standard-deviation (Sample. population-mean-difference (difference population))))
                              (/ (+ (count population-one) (count population-two)) 2)))))



;http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/stat/inference/ChiSquareTest.html


(def observed [[60 300] [10 390]])


(def total (double (esum observed)))
(def row-total (map #(reduce + %) observed))
(def column-total (map #(reduce + %) (columns observed)))

(mmul [row-total column-total])

(def expected-val '(33.1578947368421 326.8421052631579 36.8421052631579 363.1578947368421))

(def expected
  (for [rt (map #(reduce + %) observed)
        ct (map #(reduce + %) (columns observed))]
    (/ (* rt ct) (double (esum observed)))))

(esum (emap op// (emap (fn [x] (op/* x x)) (sub observed expected))
            expected))


(pearson-chi-square (Independance. 60 33.16))

;TODO seperate out observed vs expected values and use below (values) of (fn [observed expected] ...)
(defn chi-square
  "Assumes data to be in the form
  [[x1 observed, x1 expected] [x2 observed, x2 expected]].
   The Chi-square test computes the sum of the squares of the differences in values"
  [values]
  (reduce + 0
          (map
            (fn [[observed expected]]
              (double
                (/ (pow (- observed expected) 2)
                   expected)))
            values)))


(for [rt (map #(reduce + %) observed)
      ct (map #(reduce + %) (columns observed))]
  [(first (first observed)) (/ (* rt ct) (double (esum observed)))])

(for [rt (map #(reduce + %) observed)
      ct (map #(reduce + %) (columns observed))
      :let [out (atom [])
            cnt (atom (count (first observed)))]
      :when (not= @cnt (count @out))]
  (swap! out conj
         (/ (* rt ct) (double (esum observed)))))