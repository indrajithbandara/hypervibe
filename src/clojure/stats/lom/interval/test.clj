(ns clojure.stats.lom.interval.test
  (:require [clojure.stats.utils.central-tendancy :refer [mean difference]]
            [clojure.stats.utils.variation :refer [smpl-std-dev smpl-var pool-var]]
            [clojure.stats.distribution.t.table :refer [t-dist crtcl-val]]))
(use 'clojure.core.matrix)

(defprotocol Interval
  (ttest [this] "Conducts a ttest on a dataset. A t-test looks at the t-statistic, the t-distribution and
                 degrees of freedom to determine the probability of difference between populations"))

(defrecord OneSample [smpl h-mean alpha]
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (mean smpl)
                          (smpl-std-dev smpl (mean smpl))
                          (count smpl))
          [sample-mean sample-standard-deviation sample-size] pcalcs
          dof (dec sample-size)]
      (assoc type :t-stat (/ (- sample-mean h-mean)
                             (/ sample-standard-deviation
                                (Math/sqrt sample-size)))
                  :dof dof
                  :alpha alpha
                  :crtcl-val (crtcl-val t-dist dof alpha)
                  :smpl-mean sample-mean
                  :smpl-std-dev sample-standard-deviation
                  :smpl-size sample-size))))


(defrecord EqualVariance [samples h-mean alpha]
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (map mean samples)
                          (map mean (partition 1 h-mean))
                          (map #(pool-var % (mean %) (dec (count %))) samples)
                          (map count samples))
          [[sample-mean-one sample-mean-two] [population-mean-one population-mean-two]
           [pooled-variance-one pooled-variance-two] [sample-size-one sample-size-two]] pcalcs
          dof (- (+ sample-size-one sample-size-two) 2)]
      (assoc type :t-statistic (/ (- (- sample-mean-one sample-mean-two)
                                     (- population-mean-one population-mean-two))
                                  (Math/sqrt (* (/ (+ pooled-variance-one pooled-variance-two) 2)
                                                (+ (/ 1 sample-size-one) (/ 1 sample-size-two)))))
                  :dof dof
                  :alpha alpha
                  :critical-value (crtcl-val t-dist dof alpha)
                  :sample-means [sample-mean-one sample-mean-two]
                  :population-means [population-mean-one population-mean-two]
                  :pooled-variances [pooled-variance-one pooled-variance-two]
                  :sample-sizes [sample-size-one sample-size-two]))))


(defrecord Welch [samples alpha]                            ;TODO test in SPSS
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (map mean samples)
                          (map #(smpl-var % (mean %)) samples)
                          (map count samples))
          [[mean-one mean-two] [sample-variance-one sample-variance-two]
           [sample-size-one sample-size-two]] pcalcs
          dof (/ (* (+ (/ sample-variance-one sample-size-one)
                       (/ sample-variance-two sample-size-two))
                    (+ (/ sample-variance-one sample-size-one)
                       (/ sample-variance-two sample-size-two)))
                 (+ (/ (* (/ sample-variance-one sample-size-one)
                          (/ sample-variance-one sample-size-one))
                       (- sample-size-one 1))
                    (/ (* (/ sample-variance-two sample-size-two)
                          (/ sample-variance-two sample-size-two))
                       (- sample-size-two 1))))]
      (assoc type :t-statistic (/ (- mean-one mean-two)
                                  (Math/sqrt (+ (/ sample-variance-one sample-size-one)
                                                (/ sample-variance-two sample-size-two))))
                  :dof dof
                  :alpha alpha
                  :critical-value (crtcl-val t-dist (Math/round dof) alpha)
                  :sample-means [mean-one mean-two]
                  :sample-variances [sample-variance-one sample-variance-two]
                  :sample-sizes [sample-size-one sample-size-two]))))


(defrecord RepeatedMeasure [population h-mean alpha] ;TODO check population size is correct
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (mean (difference population))
                          (map mean (partition 1 h-mean))
                          (smpl-std-dev (difference population) (mean (difference population)))
                          (/ (+ (count (first population)) (count (second population))) 2))
          [difference-mean [population-mean-one population-mean-two] standard-deviation population-size] pcalcs
          dof (dec population-size)]
      (assoc type :t-statistic (/ (- difference-mean
                                     (- population-mean-one population-mean-two))
                                  (/ standard-deviation
                                     (Math/sqrt population-size)))
                  :dof dof
                  :alpha alpha
                  :critical-value (crtcl-val t-dist dof alpha)
                  :population-means [population-mean-one population-mean-two]
                  :standard-deviation standard-deviation
                  :population-size population-size
                  :difference-mean difference-mean))))


(defrecord OneSampleMedian []
  Interval
  (ttest [type]
    (println type)))

