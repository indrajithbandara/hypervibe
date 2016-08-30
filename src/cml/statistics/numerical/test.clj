(ns cml.statistics.numerical.test
  (:require [cml.utils.tables :refer [t-table]]
            [cml.utils.central-tendancy :refer [mean]]
            [cml.utils.variation :refer [standard-deviation variance]])
  (:import [cml.utils.variation Sample Pooled]))

;TODO Have functions comply with dataframes
(defprotocol Ordinal)

(defprotocol Numerical
  (t-test [tt] "Conducts a TTest on a numerical data set"))

(defrecord OneSample [sample h-mean]
  Numerical
  (t-test [type]
    (let [pcalcs (pvalues (mean sample)
                          (:standard-deviation (standard-deviation (Sample. (mean sample) sample)))
                          (count sample))
          [mean sample-standard-deviation sample-size] pcalcs]
      (assoc type
        :t-statistic (/ (- mean h-mean)
                        (/ sample-standard-deviation
                           (Math/sqrt sample-size)))
        :dof (dec sample-size)
        :sample-mean mean
        :sample-standard-deviation sample-standard-deviation
        :sample-size sample-size))))


(defrecord EqualVariance [sample h-mean]
  Numerical
  (t-test [type]
    (let [pcalcs (pvalues (map mean sample)
                          (map mean (partition 1 h-mean))
                          (map #(:variance (variance (Pooled. (mean %) % (- (count %) 1)))) sample)
                          (map count sample))
          [[mean-one mean-two] [population-mean-one population-mean-two]
           [pooled-variance-one pooled-variance-two] [sample-size-one sample-size-two]] pcalcs]
      (assoc type
        :t-statistic (/ (- (- mean-one mean-two)
                           (- population-mean-one population-mean-two))
                        (Math/sqrt (* (/ (+ pooled-variance-one pooled-variance-two) 2)
                                      (+ (/ 1 sample-size-one) (/ 1 sample-size-two)))))
        :dof (- (+ sample-size-one sample-size-two) 2)
        :sample-means [mean-one mean-two]
        :population-means [population-mean-one population-mean-two]
        :pooled-variances [pooled-variance-one pooled-variance-two]
        :sample-sizes [sample-size-one sample-size-two]))))


(defrecord Welch [mean sample-variance size]
  Numerical
  (t-test [type]
    (let [[mean-one mean-two] mean
          [sample-variance-one sample-variance-two] sample-variance
          [size-one size-two] size]
      (assoc type
        :t-statistic (/ (- mean-one mean-two)
                        (Math/sqrt (+ (/ sample-variance-one size-one)
                                      (/ sample-variance-two size-two))))
        :dof (/ (* (+ (/ sample-variance-one size-one)
                      (/ sample-variance-two size-two))
                   (+ (/ sample-variance-one size-one)
                      (/ sample-variance-two size-two)))
                (+ (/ (* (/ sample-variance-one size-one)
                         (/ sample-variance-one size-one))
                      (- size-one 1))
                   (/ (* (/ sample-variance-two size-two)
                         (/ sample-variance-two size-two))
                      (- size-two 1))))))))


(defrecord RepeatedMeasure [difference-mean population-mean standard-deviation size]
  Numerical
  (t-test [type]
    (let [[population-mean-one population-mean-two] population-mean]
      (assoc type
        :t-statistic (/ (- difference-mean
                           (- population-mean-one population-mean-two))
                        (/ standard-deviation
                           (Math/sqrt size)))
        :dof (- size 1)))))


