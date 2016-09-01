(ns cml.statistics.numerical.test
  (:require [cml.utils.tables :refer [t-table]]
            [cml.utils.central-tendancy :refer [mean difference]]
            [cml.utils.variation :refer [standard-deviation variance]])
  (:import [cml.utils.variation Sample Pooled]))

;TODO Have functions comply with dataframes

(defprotocol Numerical
  (t-test [tt] "Conducts a TTest on a numerical data set"))

(defrecord OneSample [sample h-mean]
  Numerical
  (t-test [type]
    (let [pcalcs (pvalues (mean sample)
                          (:standard-deviation (standard-deviation (Sample. (mean sample) sample)))
                          (count sample))
          [mean sample-standard-deviation sample-size] pcalcs]
      (assoc type :t-statistic (/ (- mean h-mean)
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
      (assoc type :t-statistic (/ (- (- mean-one mean-two)
                                     (- population-mean-one population-mean-two))
                                  (Math/sqrt (* (/ (+ pooled-variance-one pooled-variance-two) 2)
                                                (+ (/ 1 sample-size-one) (/ 1 sample-size-two)))))
                  :dof (- (+ sample-size-one sample-size-two) 2)
                  :sample-means [mean-one mean-two]
                  :population-means [population-mean-one population-mean-two]
                  :pooled-variances [pooled-variance-one pooled-variance-two]
                  :sample-sizes [sample-size-one sample-size-two]))))


(defrecord Welch [sample]
  Numerical
  (t-test [type]
    (let [pcalcs (pvalues (map mean sample)
                          (map #(:variance (variance (Sample. (mean %) %))) sample)
                          (map count sample))
          [[mean-one mean-two] [sample-variance-one sample-variance-two]
           [sample-size-one sample-size-two]] pcalcs]
      (assoc type :t-statistic (/ (- mean-one mean-two)
                                  (Math/sqrt (+ (/ sample-variance-one sample-size-one)
                                      (/ sample-variance-two sample-size-two))))
                  :dof (/ (* (+ (/ sample-variance-one sample-size-one)
                                (/ sample-variance-two sample-size-two))
                             (+ (/ sample-variance-one sample-size-one)
                                (/ sample-variance-two sample-size-two)))
                          (+ (/ (* (/ sample-variance-one sample-size-one)
                                   (/ sample-variance-one sample-size-one))
                                (- sample-size-one 1))
                             (/ (* (/ sample-variance-two sample-size-two)
                                   (/ sample-variance-two sample-size-two))
                                (- sample-size-two 1))))
                  :sample-means [mean-one mean-two]
                  :sample-variances [sample-variance-one sample-variance-two]
                  :sample-sizes [sample-size-one sample-size-two]))))


(defrecord RepeatedMeasure [population h-mean]
  Numerical
  (t-test [type]
    (let [pcalcs (pvalues (mean (difference population))
                          (map mean (partition 1 h-mean))
                          (:standard-deviation (standard-deviation (Sample. (mean (difference population)) (difference population))))
                          (/ (+ (count (first population)) (count (second population))) 2))
          [difference-mean [population-mean-one population-mean-two] standard-deviation population-size] pcalcs]
      (assoc type :t-statistic (/ (- difference-mean
                                     (- population-mean-one population-mean-two))
                                  (/ standard-deviation
                                     (Math/sqrt population-size)))
                  :dof (- population-size 1)
                  :population-means [population-mean-one population-mean-two]
                  :standard-deviation standard-deviation
                  :population-size population-size
                  :difference-mean difference-mean))))


