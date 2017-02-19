(ns clojure.stats.test
  (:require [clojure.stats.utils :refer [mean diff rnull? ssdev
                                         svar pvar oststat
                                         equal-var-tstat welch-tstat rmsure-tstat
                                         welch-dof]]
            [clojure.stats.distribution.t.table :refer [t-dist crtcl-val]]))
(use 'clojure.core.matrix)

(deftype OneSample [smpl hmean alpha])
(deftype EqualVariance [smpls hmeans alpha])
(deftype Welch [smpls alpha])
(deftype RepeatedMeasure [smpls hmeans alpha])
(deftype Median [smpls hmeans alpha])


(defmulti ttest class)


(defn one-sample
  [smpl hmean tstat
   dof alpha cval
   rnull? diff smean
   ssdev ssize]
  ^{:type ::OneSample}
  {:smpl   smpl
   :hmean  hmean
   :tstat  tstat
   :dof    dof
   :alpha  alpha
   :cval   cval
   :rnull? rnull?
   :diff   diff
   :smean  smean
   :ssdev  ssdev
   :ssize  ssize})


(defn equal-variance
  [smpls hmeans tstat
   dof alpha cval
   rnull? diff smeans
   pmeans pool-vars ssizes]
  ^{:type ::EqualVariance}
  {:smpls     smpls
   :hmeans    hmeans
   :tstat     tstat
   :dof       dof
   :alpha     alpha
   :cval      cval
   :rnull?    rnull?
   :diff      diff
   :smeans    smeans
   :pmeans    pmeans
   :pool-vars pool-vars
   :ssizes    ssizes})


(defn welch
  [smpls tstat dof
   alpha cval rnull?
   diff smeans svars
   ssizes]
  ^{:type ::Welch}
  {:smpls  smpls
   :tstat  tstat
   :dof    dof
   :alpha  alpha
   :cval   cval
   :rnull? rnull?
   :diff   diff
   :smeans smeans
   :svars  svars
   :ssizes ssizes})

(defn rmsure
  [smpls hmeans tstat
   dof alpha cval
   rnull? pmeans
   sdev ssize
   diff-mean]
  ^{:type ::RepeatedMeasure}
  {:smpls     smpls
   :hmeans    hmeans
   :tstat     tstat
   :dof       dof
   :alpha     alpha
   :cval      cval
   :rnull?    rnull?
   :pmeans    pmeans
   :sdev      sdev
   :ssize     ssize
   :diff-mean diff-mean})


(defn- pone-sample [this]
  (pvalues (mean (.smpl this))
           (ssdev (.smpl this) (mean (.smpl this)))
           (count (.smpl this))))

(defmethod ttest OneSample [this]
  (let [[smean ssdev ssize] (pone-sample this)
        cval (crtcl-val t-dist (dec ssize) (.alpha this))
        tstat (oststat smean this ssdev ssize)]
    (one-sample (.smpl this) (.hmean this) tstat
                (dec ssize) (.alpha this) cval
                (rnull? tstat cval) (- tstat cval) smean
                ssdev ssize)))


(defn- pequal-var [this]
  (pvalues (map mean (.smpls this))
           (map mean (partition 1 (.hmeans this)))
           (map #(pvar % (mean %) (dec (count %))) (.smpls this))
           (map count (.smpls this))))

(defmethod ttest EqualVariance [this]
  (let [[[smone smtwo] [pmone pmtwo] [pvone pvtwo]
         [ssone sstwo]] (pequal-var this)
        cval (crtcl-val t-dist (- (+ ssone sstwo) 2) (.alpha this))
        tstat (equal-var-tstat smone smtwo pmone pmtwo pvone pvtwo ssone sstwo)]
    (equal-variance (.smpls this) (.hmeans this) tstat
                    (- (+ ssone sstwo) 2) (.alpha this) cval
                    (rnull? tstat cval) (- tstat cval) [smone smtwo]
                    [pmone pmtwo] [pvone pvtwo] [ssone sstwo])))


(defn- pwelch [this]
  (pvalues (map mean (.smpls this))
           (map #(svar % (mean %)) (.smpls this))
           (map count (.smpls this))))

(defmethod ttest Welch [this]
  (let [[[mone mtwo] [svone svtwo] [ssone sstwo]] (pwelch this)
        dof (welch-dof svone svtwo ssone sstwo)
        cval (crtcl-val t-dist (Math/round dof) (.alpha this))
        tstat (welch-tstat mone mtwo svone svtwo ssone sstwo)]
    (welch (.smpls this) tstat dof
           (.alpha this) cval (rnull? tstat cval)
           (- tstat cval) [mone mtwo] [svone svtwo]
           [ssone sstwo])))


(defn- prmsure [this]
  (pvalues (mean (diff (.smpls this)))
           (map mean (partition 1 (.hmeans this)))
           (ssdev (diff (.smpls this)) (mean (diff (.smpls this))))
           (/ (+ (count (first (.smpls this))) (count (second (.smpls this)))) 2)))

(defmethod ttest RepeatedMeasure [this]
  (let [[diff-mean [pmone pmtwo] sdev ssize] (prmsure this)
        cval (crtcl-val t-dist (dec ssize) (.alpha this))
        tstat (rmsure-tstat diff-mean pmone pmtwo sdev ssize)]
    (rmsure (.smpls this) (.hmeans this) tstat
            (dec ssize) (.alpha this) cval
            (rnull? tstat cval) [pmone pmtwo] sdev
            ssize diff-mean)))



(defn chi-square
  "Assumes in to be in the form
  [[x1 observed, x1 expected] [x2 observed, x2 expected]].
   The Chi-square test computes the sum of the squares of the differences in values"
  [values]
  (reduce + 0 (map (fn [[observed expected]]
                     (double (/ (Math/pow (- observed expected) 2) expected))) values)))



(chi-square [[60 33.16]
             [300 326.84]

             [10 36.84]
             [390 363.16]])


(def data (matrix [[60 300] [10 390]]))


(def observed [60 300 10 390])
(def expected [33.16 326.84 36.84 363.16])

(def column-total (map (fn [x] (apply + x)) (columns data))) ;=>  (70 690)
(def row-total (map (fn [x] (apply + x)) data))             ;=>  (360 400)
(def grand-total (reduce + (map (fn [x] (apply + x)) data))) ;=> 760

(double (/ (* 70 360) 760))                                 ;=> 33.16
(double (/ (* 690 400) 760))                                ;=> 363.16
(double (/ (* 690 360) 760))                                ;=> 326.84
(double (/ (* 400 70) 760))                                 ;=> 36.84

;TODO create parallel chi square function.. could macro help?
