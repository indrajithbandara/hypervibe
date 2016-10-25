(ns clojure.stats.lom.interval.test
  (:require [clojure.stats.utils.tables :refer [t-table]]
            [clojure.stats.utils.central-tendancy :refer [mean difference]]
            [clojure.stats.utils.variation :refer [smpl-std-dev smpl-var pool-var]]))


(defprotocol Interval
  (ttest [this] "Conducts a ttest on a dataset. A t-test looks at the t-statistic, the t-distribution and
                 degrees of freedom to determine the probability of difference between populations"))

(defrecord OneSample [sample h-mean]
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (mean sample)
                          (smpl-std-dev sample (mean sample))
                          (count sample))
          [sample-mean sample-standard-deviation sample-size] pcalcs]
      (assoc type :t-statistic (/ (- sample-mean h-mean)
                                  (/ sample-standard-deviation
                                     (Math/sqrt sample-size)))
                  :dof (dec sample-size)
                  :sample-mean sample-mean
                  :sample-standard-deviation sample-standard-deviation
                  :sample-size sample-size))))


(defrecord EqualVariance [samples h-mean]
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (map mean samples)
                          (map mean (partition 1 h-mean))
                          (map #(pool-var % (mean %) (dec (count %))) samples)
                          (map count samples))
          [[sample-mean-one sample-mean-two] [population-mean-one population-mean-two]
           [pooled-variance-one pooled-variance-two] [sample-size-one sample-size-two]] pcalcs]
      (assoc type :t-statistic (/ (- (- sample-mean-one sample-mean-two)
                                     (- population-mean-one population-mean-two))
                                  (Math/sqrt (* (/ (+ pooled-variance-one pooled-variance-two) 2)
                                                (+ (/ 1 sample-size-one) (/ 1 sample-size-two)))))
                  :dof (- (+ sample-size-one sample-size-two) 2)
                  :sample-means [sample-mean-one sample-mean-two]
                  :population-means [population-mean-one population-mean-two]
                  :pooled-variances [pooled-variance-one pooled-variance-two]
                  :sample-sizes [sample-size-one sample-size-two]))))


(defrecord Welch [samples]
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (map mean samples)
                          (map #(smpl-var % (mean %)) samples)
                          (map count samples))
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
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (mean (difference population))
                          (map mean (partition 1 h-mean))
                          (smpl-std-dev (difference population) (mean (difference population)))
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


(defrecord OneSampleMedian []                               ;TODO consider moving all tests into one ns
  Interval
  (ttest [type]))

