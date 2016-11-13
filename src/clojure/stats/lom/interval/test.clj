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
          [smpl-mean smpl-std-dev smpl-size] pcalcs
          dof (dec smpl-size)]
      (assoc type :t-stat (/ (- smpl-mean h-mean)
                             (/ smpl-std-dev
                                (Math/sqrt smpl-size)))
                  :dof dof
                  :alpha alpha
                  :crtcl-val (crtcl-val t-dist dof alpha)
                  :smpl-mean smpl-mean
                  :smpl-std-dev smpl-std-dev
                  :smpl-size smpl-size))))


(defrecord EqualVariance [smpls h-means alpha]
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (map mean smpls)
                          (map mean (partition 1 h-means))
                          (map #(pool-var % (mean %) (dec (count %))) smpls)
                          (map count smpls))
          [[smpl-mean-one smpl-mean-two] [pop-mean-one pop-mean-two]
           [pool-var-one pool-var-two] [smpl-size-one smpl-size-two]] pcalcs
          dof (- (+ smpl-size-one smpl-size-two) 2)]
      (assoc type :t-stat (/ (- (- smpl-mean-one smpl-mean-two)
                                (- pop-mean-one pop-mean-two))
                             (Math/sqrt (* (/ (+ pool-var-one pool-var-two) 2)
                                           (+ (/ 1 smpl-size-one) (/ 1 smpl-size-two)))))
                  :dof dof
                  :alpha alpha
                  :crtcl-val (crtcl-val t-dist dof alpha)
                  :smpl-means [smpl-mean-one smpl-mean-two]
                  :pop-means [pop-mean-one pop-mean-two]
                  :pool-vars [pool-var-one pool-var-two]
                  :smpl-sizes [smpl-size-one smpl-size-two]))))


(defrecord Welch [smpls alpha]                            ;TODO test in SPSS
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (map mean smpls)
                          (map #(smpl-var % (mean %)) smpls)
                          (map count smpls))
          [[mean-one mean-two] [smpl-var-one smpl-var-two]
           [smpl-size-one smpl-size-two]] pcalcs
          dof (/ (* (+ (/ smpl-var-one smpl-size-one)
                       (/ smpl-var-two smpl-size-two))
                    (+ (/ smpl-var-one smpl-size-one)
                       (/ smpl-var-two smpl-size-two)))
                 (+ (/ (* (/ smpl-var-one smpl-size-one)
                          (/ smpl-var-one smpl-size-one))
                       (- smpl-size-one 1))
                    (/ (* (/ smpl-var-two smpl-size-two)
                          (/ smpl-var-two smpl-size-two))
                       (- smpl-size-two 1))))]
      (assoc type :t-stat (/ (- mean-one mean-two)
                                  (Math/sqrt (+ (/ smpl-var-one smpl-size-one)
                                                (/ smpl-var-two smpl-size-two))))
                  :dof dof
                  :alpha alpha
                  :crtcl-val (crtcl-val t-dist (Math/round dof) alpha)
                  :smpl-means [mean-one mean-two]
                  :smpl-vars [smpl-var-one smpl-var-two]
                  :smpl-sizes [smpl-size-one smpl-size-two]))))


(defrecord RepeatedMeasure [smpls h-means alpha]
  Interval
  (ttest [type]
    (let [pcalcs (pvalues (mean (difference smpls))
                          (map mean (partition 1 h-means))
                          (smpl-std-dev (difference smpls) (mean (difference smpls)))
                          (/ (+ (count (first smpls)) (count (second smpls))) 2))
          [diff-mean [pop-mean-one pop-mean-two] std-dev smpl-size] pcalcs
          dof (dec smpl-size)]
      (assoc type :t-stat (/ (- diff-mean
                                (- pop-mean-one pop-mean-two))
                             (/ std-dev
                                (Math/sqrt smpl-size)))
                  :dof dof
                  :alpha alpha
                  :crtcl-val (crtcl-val t-dist dof alpha)
                  :pop-means [pop-mean-one pop-mean-two]
                  :std-dev std-dev
                  :smpl-size smpl-size
                  :diff-mean diff-mean))))


(defrecord OneSampleMedian []
  Interval
  (ttest [type]
    (println type)))

