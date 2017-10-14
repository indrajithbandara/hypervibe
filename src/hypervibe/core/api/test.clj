(ns hypervibe.core.api.test
  (:require [hypervibe.core.api.utils :refer [mean ssdev diff rnull? svar pvar]]
            [clojure.core.matrix :as m]
            [hypervibe.core.api.distribution.t.table :refer [t utail]]
            [clojure.core.matrix.operators :as op]))

(defrecord OneSample [smpl hmean alpha])
(defrecord EqualVariance [smpls hmeans alpha])
(defrecord Welch [smpls alpha])
(defrecord RepeatedMeasure [smpls hmeans alpha])
(defrecord Median [smpls hmeans alpha])

(defmulti ttest class)
(defmulti tstat class)

(defmethod ttest OneSample
  [ttest]
  (->> (pvalues (mean (:smpl ttest))
                (ssdev (:smpl ttest) (mean (:smpl ttest)))
                (m/ecount (:smpl ttest)))
       (zipmap [:smean :ssdev :ssize])
       (#(assoc ttest :smean (:smean %1)
                      :ssdev (:ssdev %1)
                      :ssize (:ssize %1)
                      :alpha (:alpha ttest)
                      :dof (dec (:ssize %1))
                      :diff nil))))

(defmethod tstat OneSample
  [tstat]
  (assoc tstat :tstat
               (/ (- (:smean tstat)
                     (:hmean tstat))
                  (/ (:ssdev tstat)
                     (Math/sqrt (:ssize tstat))))))

(defmethod ttest EqualVariance
  [ttest]
  (->> (pvalues (mapv mean (:smpls ttest))
                (mapv mean (partition 1 (:hmeans ttest)))
                (mapv #(pvar % (mean %) (dec (count %))) (:smpls ttest))
                (mapv count (:smpls ttest)))
       (zipmap [:smeans :pmeans :pvars :ssizes])
       (#(assoc ttest :smeans (:smeans %1)
                      :pmeans (:pmeans %1)
                      :pvars (:pvars %1)
                      :ssizes (:ssizes %1)
                      :dof (- (apply + (:ssizes %1)) 2)
                      :diff nil))))

(defmethod tstat EqualVariance
  [tstat]
  (assoc tstat :tstat
               (/ (- (- ((:smeans tstat) 0)
                        ((:smeans tstat) 1))
                     (- ((:pmeans tstat) 0)
                        ((:pmeans tstat) 1)))
                  (Math/sqrt (* (/ (+ ((:pvars tstat) 0)
                                      ((:pvars tstat) 1)) 2)
                                (+ (/ 1 ((:ssizes tstat) 0))
                                   (/ 1 ((:ssizes tstat) 1))))))))

(defmethod ttest Welch
  [ttest]
  (->> (pvalues (mapv mean (:smpls ttest))
                (mapv #(svar % (mean %)) (:smpls ttest))
                (mapv count (:smpls ttest)))
       (zipmap [:smeans :svars :ssizes])
       (#(assoc ttest :smeans (:smeans %1)
                      :svars (:svars %1)
                      :ssizes (:ssizes %1)
                      :alpha (:alpha ttest)
                      :dof (/ (* (+ (/ ((:svars %1) 0)
                                       ((:ssizes %1) 0))
                                    (/ ((:svars %1) 1)
                                       ((:ssizes %1) 1)))
                                 (+ (/ ((:svars %1) 0)
                                       ((:ssizes %1) 0))
                                    (/ ((:svars %1) 1)
                                       ((:ssizes %1) 1))))
                              (+ (/ (* (/ ((:svars %1) 0)
                                          ((:ssizes %1) 0))
                                       (/ ((:svars %1) 0)
                                          ((:ssizes %1) 0)))
                                    (- ((:ssizes %1) 0) 1))
                                 (/ (* (/ ((:svars %1) 1)
                                          ((:ssizes %1) 1))
                                       (/ ((:svars %1) 1)
                                          ((:ssizes %1) 1)))
                                    (- ((:ssizes %1) 1) 1))))))))

(defmethod tstat Welch
  [tstat]
  (assoc tstat :tstat
               (/ (- ((:smeans tstat) 0)
                     ((:smeans tstat) 1))
                  (Math/sqrt (+ (/ ((:svars tstat) 0)
                                   ((:ssizes tstat) 0))
                                (/ ((:svars tstat) 1)
                                   ((:ssizes tstat) 1)))))))

(defmethod ttest RepeatedMeasure
  [ttest]
  (->> (pvalues (mean (diff (:smpls ttest)))
                (mapv mean (partition 1 (:hmeans ttest)))
                (ssdev (diff (:smpls ttest))
                       (-> (:smpls ttest) diff mean))
                (/ (+ (count (first (:smpls ttest)))
                      (count (second (:smpls ttest)))) 2))
       (zipmap [:dmean :means :sdev :ssize])
       (#(assoc ttest :means (:means %1)
                      :dmean (:dmean %1)
                      :sdev (:sdev %1)
                      :ssize (:ssize %1)
                      :dof (dec (:ssize %1))))))

(defmethod tstat RepeatedMeasure
  [tstat]
  (assoc tstat :tstat
               (/ (- (:dmean tstat)
                     (- ((:means tstat) 0) ((:means tstat) 1)))
                  (/ (:sdev tstat)
                     (Math/sqrt (:ssize tstat))))))


