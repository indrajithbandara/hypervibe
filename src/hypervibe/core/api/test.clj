(ns hypervibe.core.api.test
  (:require [hypervibe.core.api.utils :refer [mean ssdev diff rnull? svar pvar]]
            [hypervibe.core.api.distribution.t.table :refer [t utail]]))
(use 'clojure.core.matrix.operators)
(use 'clojure.core.matrix)

(defrecord OneSample [smpl hmean alpha])
(defrecord EqualVariance [smpls hmeans alpha])
(defrecord Welch [smpls alpha])
(defrecord RepeatedMeasure [smpls hmeans alpha])
(defrecord Median [smpls hmeans alpha])

(defmulti disc "Computes descriptive statistics" class)
(defmulti tstat "Computes test statistics" class)

(defmethod disc OneSample
  [ttest]
  (->>
    (pvalues (ecount (:smpl ttest))
             (mean (:smpl ttest))
             (ssdev (:smpl ttest)
                    (mean (:smpl ttest))))
    ((fn
       [pv]
       (cons (dec (first pv))
             pv)))
    (apply (fn
             [dof ssize smean
              ssdev]
             (assoc ttest
               :smean smean
               :ssdev ssdev
               :ssize ssize
               :alpha (:alpha ttest)
               :dof dof
               :diff nil)))))

(defmethod tstat OneSample
  [tstat]
  (assoc tstat
    :tstat
    (/ (- (:smean tstat)
          (:hmean tstat))
       (/ (:ssdev tstat)
          (Math/sqrt (:ssize tstat))))))

(defmethod disc EqualVariance
  [ttest]
  (->>
    (pvalues (mapv ecount
                   (:smpls ttest))
             (mapv mean
                   (:smpls ttest))
             (mapv mean
                   (partition 1
                              (:hmeans ttest)))
             (mapv #(pvar %
                          (mean %)
                          (dec (ecount %)))
                   (:smpls ttest)))
    ((fn
       [pv]
       (cons (- (reduce +
                        (first pv))
                2)
             pv)))
    (apply (fn
             [dof ssizes smeans
              pmeans pvars]
             (assoc ttest
               :smeans smeans
               :pmeans pmeans
               :pvars pvars
               :ssizes ssizes
               :dof dof
               :diff nil)))))

(defmethod tstat EqualVariance
  [tstat]
  (->>
    tstat
    ((fn
       [{smeans :smeans
         pmeans :pmeans
         pvars  :pvars
         ssizes :ssizes}]
       (assoc tstat
         :tstat
         (/ (- (- (smeans 0)
                  (smeans 1))
               (- (pmeans 0)
                  (pmeans 1)))
            (Math/sqrt (* (/ (+ (pvars 0)
                                (pvars 1))
                             2)
                          (+ (/ 1
                                (ssizes 0))
                             (/ 1
                                (ssizes 1)))))))))))

(defmethod disc Welch
  [ttest]
  (->>
    (pvalues (mapv mean
                   (:smpls ttest))
             (mapv #(svar %
                          (mean %))
                   (:smpls ttest))
             (mapv ecount
                   (:smpls ttest)))
    ((fn
       [[[smone smtwo]
         [svone svtwo]
         [szone sztwo]
         :as all]]
       (cons (/ (* (+ (/ svone
                         szone)
                      (/ svtwo
                         sztwo))
                   (+ (/ svone
                         szone)
                      (/ svtwo
                         sztwo)))
                (+ (/ (* (/ svone
                            szone)
                         (/ svone
                            szone))
                      (- szone 1))
                   (/ (* (/ svtwo
                            sztwo)
                         (/ svtwo
                            sztwo))
                      (- sztwo
                         1))))
             all)))
    (apply (fn
             [dof smeans svars ssizes]
             (assoc ttest
               :smeans smeans
               :svars svars
               :ssizes ssizes
               :alpha (:alpha ttest)
               :dof dof)))))

(defmethod tstat Welch
  [tstat]
  (assoc tstat
    :tstat
    (/ (- ((:smeans tstat) 0)
          ((:smeans tstat) 1))
       (Math/sqrt (+ (/ ((:svars tstat) 0)
                        ((:ssizes tstat) 0))
                     (/ ((:svars tstat) 1)
                        ((:ssizes tstat) 1)))))))


(defmethod disc RepeatedMeasure
  [ttest]
  (->>
    (pvalues (apply (fn [sone stwo]
                      (/ (+ sone
                            stwo)
                         2))
                    (map ecount
                         (:smpls ttest)))
             (mean (diff (:smpls ttest)))
             (mapv mean
                   (partition 1
                              (:hmeans ttest)))
             (ssdev (diff (:smpls ttest))
                    (->
                      (:smpls ttest)
                      diff
                      mean)))
    ((fn
       [pv]
       (cons (dec (first
                    pv))
             pv)))
    (apply (fn [dof ssize dmean
                means sdev]
             (assoc ttest
               :means means
               :dmean dmean
               :sdev sdev
               :ssize ssize
               :dof dof)))))

(defmethod tstat RepeatedMeasure
  [tstat]
  (assoc tstat
    :tstat
    (/ (- (:dmean tstat)
          (- ((:means tstat)
               0)
             ((:means tstat)
               1)))
       (/ (:sdev tstat)
          (Math/sqrt (:ssize tstat))))))


