(ns cml.core.statistics.numerical.test
  (:require [cml.statistics.numerical.test :refer [t-test]]
            [cml.statistics.categorical.test :refer [pearson-chi-square]]
            [cml.utils.variation :refer [standard-deviation variance]]
            [cml.utils.central-tendancy :refer [mean difference]])
  (:import [cml.statistics.numerical.test OneSample EqualVariance Welch RepeatedMeasure]
           [cml.utils.variation Sample Pooled]))

;TODO seperate out tests into differnt packages depending on the data types eg numerical/categorocal etc

(defn one-sample-ttest [{:keys [sample h-mean]}]
  (t-test (OneSample. sample h-mean)))


(defn equal-var-ttest [{:keys [sample h-mean]}]
  (t-test (EqualVariance. sample h-mean)))


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


