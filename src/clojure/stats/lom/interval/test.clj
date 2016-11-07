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


(defrecord EqualVariance [smpls h-mean alpha]
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (map mean smpls)
                          (map mean (partition 1 h-mean))
                          (map #(pool-var % (mean %) (dec (count %))) smpls)
                          (map count smpls))
          [[sample-mean-one sample-mean-two] [population-mean-one population-mean-two]
           [pooled-variance-one pooled-variance-two] [sample-size-one sample-size-two]] pcalcs
          dof (- (+ sample-size-one sample-size-two) 2)]
      (assoc type :t-stat (/ (- (- sample-mean-one sample-mean-two)
                                (- population-mean-one population-mean-two))
                             (Math/sqrt (* (/ (+ pooled-variance-one pooled-variance-two) 2)
                                           (+ (/ 1 sample-size-one) (/ 1 sample-size-two)))))
                  :dof dof
                  :alpha alpha
                  :crtcl-val (crtcl-val t-dist dof alpha)
                  :smpl-means [sample-mean-one sample-mean-two]
                  :pop-means [population-mean-one population-mean-two]
                  :pool-vars [pooled-variance-one pooled-variance-two]
                  :smpl-sizes [sample-size-one sample-size-two]))))


(defrecord Welch [smpls alpha]                            ;TODO test in SPSS
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (map mean smpls)
                          (map #(smpl-var % (mean %)) smpls)
                          (map count smpls))
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
      (assoc type :t-stat (/ (- mean-one mean-two)
                                  (Math/sqrt (+ (/ sample-variance-one sample-size-one)
                                                (/ sample-variance-two sample-size-two))))
                  :dof dof
                  :alpha alpha
                  :crtcl-val (crtcl-val t-dist (Math/round dof) alpha)
                  :smpl-means [mean-one mean-two]
                  :smpl-vars [sample-variance-one sample-variance-two]
                  :smpl-sizes [sample-size-one sample-size-two]))))


(defrecord RepeatedMeasure [pops h-mean alpha] ;TODO check population size is correct
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (mean (difference pops))
                          (map mean (partition 1 h-mean))
                          (smpl-std-dev (difference pops) (mean (difference pops)))
                          (/ (+ (count (first pops)) (count (second pops))) 2))
          [difference-mean [population-mean-one population-mean-two] standard-deviation population-size] pcalcs
          dof (dec population-size)]
      (assoc type :t-stat (/ (- difference-mean
                                (- population-mean-one population-mean-two))
                             (/ standard-deviation
                                (Math/sqrt population-size)))
                  :dof dof
                  :alpha alpha
                  :crtcl-val (crtcl-val t-dist dof alpha)
                  :pop-means [population-mean-one population-mean-two]
                  :std-dev standard-deviation
                  :pop-size population-size
                  :diff-mean difference-mean))))


(defrecord OneSampleMedian []
  Interval
  (ttest [type]
    (println type)))

