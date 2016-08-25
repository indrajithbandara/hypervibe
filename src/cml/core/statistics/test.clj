(ns cml.core.statistics.test
  (:require [cml.statistics.test :refer [t-test pearson-chi-square]]
            [cml.utils.variation :refer [standard-deviation variance]]
            [cml.utils.central-tendancy :refer [mean difference]])
  (:import [cml.statistics.test OneSample EqualVariance Welch RepeatedMeasure Independance]
           [cml.utils.variation Sample Pooled]))

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



