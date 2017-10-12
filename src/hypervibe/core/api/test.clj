(ns hypervibe.core.api.test
  (:require [hypervibe.core.api.utils :refer [mean ssdev diff rnull? svar pvar]]
            [clojure.core.matrix :as m]
            [hypervibe.core.api.distribution.t.table :refer [t utail]]
            [clojure.core.matrix.operators :as op]))

;TODO rename file ttest

(deftype OneSample [smpl hmean alpha])
(deftype EqualVariance [smpls hmeans alpha])
(deftype Welch [smpls alpha])
(deftype RepeatedMeasure [smpls hmeans alpha])
(deftype Median [smpls hmeans alpha])


(defrecord _OneSample [smpl hmean])
(defrecord _EqualVariance [smpls hmeans])
(defrecord _Welch [smpls alpha])
(defrecord _RepeatedMeasure [smpls hmeans alpha])
(defrecord _Median [smpls hmeans alpha])

;TODO Implement function for looking up Pvalue
;TODO use zac tellman primitive math lib

(defmulti _ttest class)
(defmulti _tstat class)

(defmethod _ttest _OneSample
  [ttest]
  (as-> (->> (pvalues (mean (:smpl ttest))
                      (ssdev (:smpl ttest) (mean (:smpl ttest)))
                      (m/ecount (:smpl ttest)))
             (zipmap [:smean :ssdev :ssize]))
        pttest
        (assoc ttest
          :smean (:smean pttest)
          :ssdev (:ssdev pttest)
          :ssize (:ssize pttest)
          :tstat (/ (- (:smean pttest)
                       (:hmean ttest))
                    (/ (:ssdev pttest)
                       (Math/sqrt (:ssize pttest))))
          :dof (dec (:ssize pttest)))))

(defmethod _tstat _OneSample
  [os-tstat]
  (assoc os-tstat
    :tstat
    (/ (- (:smean os-tstat)
          (:hmean os-tstat))
       (/ (:ssdev os-tstat)
          (Math/sqrt (:ssize os-tstat))))))

;TODO spec this function
(defn os-ttest [{smpl :smpl hmean :hmean :or {hmean 0}}] (_OneSample. smpl hmean))

(defmethod _ttest _EqualVariance
  [ttest]
  ((comp #(assoc %
            :in ttest
            :tstat (/ (- (- ((:smeans %) 0)
                            ((:smeans %) 1))
                         (- ((:pmeans %) 0)
                            ((:pmeans %) 1)))
                      (Math/sqrt (* (/ (+ ((:pvars %) 0)
                                          ((:pvars %) 1))
                                       2)
                                    (+ (/ 1
                                          ((:ssizes %) 0))
                                       (/ 1
                                          ((:ssizes %) 1))))))))
    (->> (pvalues (mapv mean
                        (:smpls ttest))
                  (mapv mean
                        (partition 1
                                   (:hmeans ttest)))
                  (mapv #(pvar %
                               (mean %)
                               (dec (count %)))
                        (:smpls ttest))
                  (mapv count
                        (:smpls ttest)))
         (zipmap [:smeans :pmeans :pvars :ssizes]))))

(defn ev-ttest [{smpls :smpls hmeans :hmeans :or {hmeans [0 0]}}] (_EqualVariance. smpls hmeans))

(defmethod _ttest _Welch                                    ;TODO fix
  [ttest]
  ((comp #(assoc %
            :in ttest
            :tstat ((fn [{[mone mtwo]   :smeans
                          [svone svtwo] :svars
                          [ssone sstwo] :ssizes}]
                      (/ (- mone
                            mtwo)
                         (Math/sqrt (+ (/ svone
                                          ssone)
                                       (/ svtwo
                                          sstwo)))))
                     %)
            :dof ((fn [[svone ssone]
                       [svtwo sstwo]]
                    (/ (* (+ (/ svone
                                ssone)
                             (/ svtwo
                                sstwo))
                          (+ (/ svone
                                ssone)
                             (/ svtwo
                                sstwo)))
                       (+ (/ (* (/ svone
                                   ssone)
                                (/ svone
                                   ssone))
                             (- ssone
                                1))
                          (/ (* (/ svtwo
                                   sstwo)
                                (/ svtwo
                                   sstwo))
                             (- sstwo
                                1)))))
                   %))
         (->> (pvalues (map mean
                            (:smpls ttest))
                       (map #(svar %
                                   (mean %))
                            (:smpls ttest))
                       (map count
                            (:smpls ttest)))
              (zipmap [:means :svars :ssizes])))))


(defmulti ttest class)

(defn one-sample-ttest
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


(defn equal-var-ttest
  [smpls hmeans tstat
   dof alpha cval
   rnull? diff smeans
   means pool-vars ssizes]
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
   :means     means
   :pool-vars pool-vars
   :ssizes    ssizes})


(defn welch-ttest
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

(defn rmsure-ttest
  [smpls hmeans tstat
   dof alpha cval
   rnull? means
   sdev ssize
   dmean]
  ^{:type ::RepeatedMeasure}
  {:smpls  smpls
   :hmeans hmeans
   :tstat  tstat
   :dof    dof
   :alpha  alpha
   :cval   cval
   :rnull? rnull?
   :means  means
   :sdev   sdev
   :ssize  ssize
   :dmean  dmean})

(defmethod ttest OneSample [this]
  (let [[smean ssdev ssize]
        (pvalues (mean (.smpl this))
                 (ssdev (.smpl this)
                        (mean (.smpl this)))
                 (m/ecount (.smpl this)))
        cval (utail (t {:Ptile (.alpha this)
                        :dof   (dec ssize)}))
        tstat (/ (- smean (.hmean this))
                 (/ ssdev (Math/sqrt ssize)))]
    (one-sample-ttest (.smpl this)
                      (.hmean this)
                      tstat
                      (dec ssize)
                      (.alpha this)
                      cval
                      (rnull? tstat cval)
                      (- tstat cval)
                      smean
                      ssdev
                      ssize)))

(defmethod ttest EqualVariance [this]
  (let [[[smone smtwo]
         [pmone pmtwo]
         [pvone pvtwo]
         [ssone sstwo]]
        (pvalues (map mean
                      (.smpls this))
                 (map mean
                      (partition 1
                                 (.hmeans this)))
                 (map #(pvar %
                             (mean %)
                             (dec (count %)))
                      (.smpls this))
                 (map count
                      (.smpls this)))
        cval (utail (t {:Ptile (.alpha this)
                        :dof   (- (+ ssone sstwo)
                                  2)}))
        tstat (/ (- (- smone smtwo)
                    (- pmone pmtwo))
                 (Math/sqrt (* (/ (+ pvone pvtwo)
                                  2)
                               (+ (/ 1 ssone)
                                  (/ 1 sstwo)))))]
    (equal-var-ttest (.smpls this)
                     (.hmeans this)
                     tstat
                     (- (+ ssone sstwo) 2)
                     (.alpha this) cval
                     (rnull? tstat cval)
                     (- tstat cval) [smone smtwo]
                     [pmone pmtwo]
                     [pvone pvtwo]
                     [ssone sstwo])))

(defmethod ttest Welch [this]
  (let [[[mone mtwo]
         [svone svtwo]
         [ssone sstwo]]
        (pvalues (map mean
                      (.smpls this))
                 (map #(svar %
                             (mean %))
                      (.smpls this))
                 (map count (.smpls this)))
        dof (/ (* (+ (/ svone ssone)
                     (/ svtwo sstwo))
                  (+ (/ svone ssone)
                     (/ svtwo sstwo)))
               (+ (/ (* (/ svone ssone)
                        (/ svone ssone))
                     (- ssone 1))
                  (/ (* (/ svtwo sstwo)
                        (/ svtwo sstwo))
                     (- sstwo 1))))
        cval (utail (t {:Ptile (.alpha this)
                        :dof   (Math/round dof)}))
        tstat (/ (- mone mtwo)
                 (Math/sqrt (+ (/ svone ssone)
                               (/ svtwo sstwo))))]
    (welch-ttest (.smpls this)
                 tstat
                 dof
                 (.alpha this)
                 cval
                 (rnull? tstat cval)
                 (- tstat cval)
                 [mone mtwo]
                 [svone svtwo]
                 [ssone sstwo])))

(defmethod ttest RepeatedMeasure [this]
  (let [[dmean
         [pmone pmtwo]
         sdev
         ssize]
        (pvalues (mean (diff (.smpls this)))
                 (map mean
                      (partition 1
                                 (.hmeans this)))
                 (ssdev (diff (.smpls this))
                        (-> (.smpls this)
                            diff
                            mean))
                 (/ (+ (count (first (.smpls this)))
                       (count (second (.smpls this))))
                    2))
        cval (utail (t {:Ptile (.alpha this)
                        :dof   (dec ssize)}))
        tstat (/ (- dmean
                    (- pmone pmtwo))
                 (/ sdev
                    (Math/sqrt ssize)))]
    (rmsure-ttest (.smpls this)
                  (.hmeans this)
                  tstat
                  (dec ssize)
                  (.alpha this)
                  cval
                  (rnull? tstat cval)
                  [pmone pmtwo]
                  sdev
                  ssize
                  dmean)))


