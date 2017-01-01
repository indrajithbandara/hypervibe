(ns clojure.stats.test
  (:require [clojure.stats.utils :refer [mean difference rej-null? smpl-std-dev smpl-var pool-var one-smpl-tstat equal-var-tstat
                                         welch-tstat rep-msure-tstat welch-dof]]
            [clojure.stats.distribution.t.table :refer [t-dist crtcl-val]]))
(use 'clojure.core.matrix)


(str (symbol (str (namespace-munge *ns*) "." name)) "/create")

(deftype OneSample [smpl hmean alpha])
(deftype EqualVariance [smpls hmeans alpha])
(deftype Welch [smpls alpha])
(deftype RepeatedMeasure [smpls hmeans alpha])
(deftype Median [smpls hmeans alpha])

(defmulti ttest class)

;TODO create light weight macro that returns type outside

(defn OneSampleTTest
  [smpl hmean tstat
   dof alpha crtcl-val
   rej-null? diff smpl-mean
   smpl-std-dev smpl-size]
  `clojure.stats.test.OneSample{:smpl         smpl
                               :hmean        hmean
   :tstat        tstat
   :dof          dof
   :alpha        alpha
   :crtcl-val    crtcl-val
   :rej-null?    rej-null?
   :diff         diff
   :smpl-mean    smpl-mean
   :smpl-std-dev smpl-std-dev
   :smpl-size    smpl-size
   :type         :OneSample})


(defn EqualVarianceTTest
  [smpls hmeans tstat
   dof alpha crtcl-val
   rej-null? diff smpl-means
   pop-means pool-vars smpl-sizes]
  (assoc `clojure.stats.test.EqualVariance{}
    :smpls smpls
    :hmeans hmeans
    :tstat tstat
    :dof dof
    :alpha alpha
    :crtcl-val crtcl-val
    :rej-null? rej-null?
    :diff diff
    :smpl-means smpl-means
    :pop-means pop-means
    :pool-vars pool-vars
    :smpl-sizes smpl-sizes
    :type :EqualVariance))


(defmethod ttest OneSample [this]
  (let [pcalcs (pvalues (mean (.smpl this))
                        (smpl-std-dev (.smpl this) (mean (.smpl this)))
                        (count (.smpl this)))
        [smpl-mean smpl-std-dev smpl-size] pcalcs
         crtcl-val (crtcl-val t-dist (dec smpl-size) (.alpha this))
         tstat (one-smpl-tstat smpl-mean this smpl-std-dev smpl-size)]
    (OneSampleTTest (.smpl this) (.hmean this) tstat
                    (dec smpl-size) (.alpha this) crtcl-val
                    (rej-null? tstat crtcl-val) (- tstat crtcl-val) smpl-mean
                     smpl-std-dev smpl-size)))


(defmethod ttest EqualVariance [this]
  (let [pcalcs (pvalues (map mean (.smpls this))
                        (map mean (partition 1 (.hmeans this)))
                        (map #(pool-var % (mean %) (dec (count %))) (.smpls this))
                        (map count (.smpls this)))
        [[smpl-mean-one smpl-mean-two] [pop-mean-one pop-mean-two] [pool-var-one pool-var-two]
         [smpl-size-one smpl-size-two]] pcalcs
          crtcl-val (crtcl-val t-dist (- (+ smpl-size-one smpl-size-two) 2) (.alpha this))
          tstat (equal-var-tstat smpl-mean-one smpl-mean-two pop-mean-one
                                 pop-mean-two pool-var-one pool-var-two
                                 smpl-size-one smpl-size-two)]
    {:smpls (.smpls this)
     :hmeans (.hmeans this)
     :tstat tstat
     :dof (- (+ smpl-size-one smpl-size-two) 2)
     :alpha (.alpha this)
     :crtcl-val crtcl-val
     :rej-null? (rej-null? tstat crtcl-val)
     :diff (- tstat  crtcl-val)
     :smpl-means [smpl-mean-one smpl-mean-two]
     :pop-means [pop-mean-one pop-mean-two]
     :pool-vars [pool-var-one pool-var-two]
     :smpl-sizes [smpl-size-one smpl-size-two]
     :type #clojure.stats.test.EqualVariance}))


(defmethod ttest Welch [this]
  (let [pcalcs (pvalues (map mean (.smpls this))
                        (map #(smpl-var % (mean %)) (.smpls this))
                        (map count (.smpls this)))
        [[mean-one mean-two] [smpl-var-one smpl-var-two] [smpl-size-one smpl-size-two]] pcalcs
          dof (welch-dof smpl-var-one smpl-var-two smpl-size-one
                         smpl-size-two)
          crtcl-val (crtcl-val t-dist (Math/round dof) (.alpha this))
          tstat (welch-tstat mean-one mean-two smpl-var-one
                              smpl-var-two smpl-size-one smpl-size-two)]
    {:smpls (.smpls this)
     :tstat tstat
     :dof dof
     :alpha (.alpha this)
     :crtcl-val crtcl-val
     :rej-null? (rej-null? tstat crtcl-val)
     :diff (- tstat crtcl-val)
     :smpl-means [mean-one mean-two]
     :smpl-vars [smpl-var-one smpl-var-two]
     :smpl-sizes [smpl-size-one smpl-size-two]
     :type clojure.stats.test.Welch}))


(defmethod ttest RepeatedMeasure [this]
  (let [pcalcs (pvalues (mean (difference (.smpls this)))
                        (map mean (partition 1 (.hmeans this)))
                        (smpl-std-dev (difference (.smpls this)) (mean (difference (.smpls this))))
                        (/ (+ (count (first (.smpls this))) (count (second (.smpls this)))) 2))
        [diff-mean [pop-mean-one pop-mean-two] std-dev smpl-size] pcalcs
         crtcl-val (crtcl-val t-dist (dec smpl-size) (.alpha this))
         tstat (rep-msure-tstat diff-mean pop-mean-one pop-mean-two
                                std-dev smpl-size)]
    {:smpls (.smpls this)
     :hmeans (.hmeans this)
     :tstat tstat
     :dof (dec smpl-size)
     :alpha (.alpha this)
     :crtcl-val crtcl-val
     :rej-null? (rej-null? tstat crtcl-val)
     :pop-means [pop-mean-one pop-mean-two]
     :std-dev std-dev
     :smpl-size smpl-size
     :diff-mean diff-mean
     :type clojure.stats.test.RepeatedMeasure}))



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
