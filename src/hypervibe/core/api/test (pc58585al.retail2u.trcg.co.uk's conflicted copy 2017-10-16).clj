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


(defrecord _OneSample [smpl hmean alpha])
(defrecord _EqualVariance [smpls hmeans alpha])
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
        (assoc ttest :smean (:smean pttest)
                     :ssdev (:ssdev pttest)
                     :ssize (:ssize pttest)
                     :alpha (:alpha ttest)
                     :tstat (/ (- (:smean pttest)
                                  (:hmean ttest))
                               (/ (:ssdev pttest)
                                  (Math/sqrt (:ssize pttest))))
                     :dof (dec (:ssize pttest)))))

(defmethod _tstat _OneSample
  [tstat]
  (assoc tstat :tstat
               (/ (- (:smean tstat)
                     (:hmean tstat))
                  (/ (:ssdev tstat)
                     (Math/sqrt (:ssize tstat))))))

;TODO spec this function

(defn os-ttest [{smpl :smpl hmean :hmean alpha :alpha :or {hmean 0 alpha 0.05}}] (_OneSample. smpl hmean alpha))

;(_tstat (_ttest (os-ttest {:smpl population-one :hmean 400})))

(defmethod _ttest _EqualVariance                            ;TODO change to mikera.vectors
  [ttest]
  (as-> (->> (pvalues (mapv mean (:smpls ttest))
                      (mapv mean (partition 1 (:hmeans ttest)))
                      (mapv #(pvar % (mean %) (dec (count %))) (:smpls ttest))
                      (mapv count (:smpls ttest)))
             (zipmap [:smeans :pmeans :pvars :ssizes]))
        pttest
        (assoc ttest :smeans (:smeans pttest)
                     :pmeans (:pmeans pttest)
                     :pvars (:pvars pttest)
                     :ssizes (:ssizes pttest)
                     :dof (- (apply + (:ssizes pttest)) 2))))

(defmethod _tstat _EqualVariance
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

;TODO spec this function
;(_tstat (_ttest (ev-ttest {:smpls [ballet-dancers football-players]})))

(defn ev-ttest [{smpls :smpls hmeans :hmeans alpha :alpha :or {hmeans [0 0] alpha 0.05}}] (_EqualVariance. smpls hmeans alpha))

(def ballet-dancers [89.2 78.2 89.3 88.3 87.3 90.1 95.2 94.3 78.3 89.3])
(def football-players [79.3 78.3 85.3 79.3 88.9 91.2 87.2 89.2 93.3 79.9])


(defmethod _ttest _Welch
  [ttest]
  (as-> (->> (pvalues (mapv mean (:smpls ttest))
                      (mapv #(svar % (mean %)) (:smpls ttest))
                      (mapv count (:smpls ttest)))
             (zipmap [:smeans :svars :ssizes]))
        pttest
        (assoc ttest :smeans (:smeans pttest)
                     :svars (:svars pttest)
                     :ssizes (:ssizes pttest)
                     :alpha (:alpha ttest)
                     :dof (/ (* (+ (/ ((:svars pttest) 0)
                                      ((:ssizes pttest) 0))
                                   (/ ((:svars pttest) 1)
                                      ((:ssizes pttest) 1)))
                                (+ (/ ((:svars pttest) 0)
                                      ((:ssizes pttest) 0))
                                   (/ ((:svars pttest) 1)
                                      ((:ssizes pttest) 1))))
                             (+ (/ (* (/ ((:svars pttest) 0)
                                         ((:ssizes pttest) 0))
                                      (/ ((:svars pttest) 0)
                                         ((:ssizes pttest) 0)))
                                   (- ((:ssizes pttest) 0) 1))
                                (/ (* (/ ((:svars pttest) 1)
                                         ((:ssizes pttest) 1))
                                      (/ ((:svars pttest) 1)
                                         ((:ssizes pttest) 1)))
                                   (- ((:ssizes pttest) 1) 1)))))))

(defmethod _tstat _Welch
  [tstat]
  (assoc tstat :tstat
               (/ (- ((:smeans tstat) 0)
                     ((:smeans tstat) 1))
                  (Math/sqrt (+ (/ ((:svars tstat) 0)
                                   ((:ssizes tstat) 0))
                                (/ ((:svars tstat) 1)
                                   ((:ssizes tstat) 1)))))))

(defn _welch-ttest [{smpls :smpls alpha :alpha :or {alpha 0.05}}] (_Welch. smpls alpha))

;(_tstat (_ttest (_welch-ttest {:smpls [ballet-dancers football-players]})))


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


