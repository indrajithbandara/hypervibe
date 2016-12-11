(ns clojure.stats.test
  (:require [clojure.stats.utils.central_tendancy :refer [mean difference]]
            [clojure.stats.utils.variation :refer [smpl-std-dev smpl-var pool-var]]
            [clojure.stats.distribution.t.table :refer [t-dist crtcl-val]]))
(use 'clojure.core.matrix)


(deftype OneSample [smpl h-mean alpha])
(deftype EqualVariance [smpls h-means alpha])
(deftype Welch [smpls alpha])
(deftype RepeatedMeasure [smpls h-means alpha])
(deftype Median [smpls h-means alpha])
(deftype Hypothesis [test])

(defrecord OneSampleTTest [smpl h-mean t-stat dof alpha crtcl-val smpl-mean smpl-std-dev smpl-size])
(defrecord EqualVarianceTTest [])
(defrecord WelchTTest [])
(defrecord RepeatedMeasureTTest [])
(defrecord OneSampleHypothesisTest [reject? smpl h-mean t-stat dof alpha crtcl-val smpl-mean smpl-std-dev smpl-size])

(defmulti ttest class)
(defmulti hypothesis-test class)
(defmulti alternative class)

(defmethod hypothesis-test OneSample [this]
  (OneSampleHypothesisTest.

    (.smpl this)
    (.h-mean this)
    (.t-stat this)
    (.dof this)
    (.alpha this)
    (.crtcl-val this)
    (.smpl-mean this)
    (.smpl-std-dev this)
    (.smpl-size this)))

(defmethod ttest OneSample [this]
  (let [pcalcs (pvalues (mean (.smpl this))
                        (smpl-std-dev (.smpl this) (mean (.smpl this)))
                        (count (.smpl this))
                        (.h-mean this))
        [smpl-mean smpl-std-dev smpl-size h-mean] pcalcs
         dof (dec smpl-size)
         alpha (or (.alpha this) 0.05)]
    (OneSampleTTest.
      (.smpl this)
       h-mean
      (/ (- smpl-mean h-mean) (/ smpl-std-dev (Math/sqrt smpl-size)))
       dof
       alpha
      (crtcl-val t-dist dof alpha)
       smpl-mean
       smpl-std-dev
       smpl-size)))


(defmethod ttest EqualVariance [this]
  (let [pcalcs (pvalues (map mean (.smpls this))
                        (map mean (partition 1 (.h-means this)))
                        (map #(pool-var % (mean %) (dec (count %))) (.smpls this))
                        (map count (.smpls this)))
        [[smpl-mean-one smpl-mean-two] [pop-mean-one pop-mean-two] [pool-var-one pool-var-two]
         [smpl-size-one smpl-size-two]] pcalcs
          dof (- (+ smpl-size-one smpl-size-two) 2)
          alpha (.alpha this)]
    (assoc {}
      :smpls (.smpls this)
      :h-means (.h-means this)
      :t-stat (/ (- (- smpl-mean-one smpl-mean-two)
                    (- pop-mean-one pop-mean-two))
                 (Math/sqrt (* (/ (+ pool-var-one pool-var-two) 2)
                               (+ (/ 1 smpl-size-one)
                                  (/ 1 smpl-size-two)))))
      :dof dof
      :alpha alpha
      :crtcl-val (crtcl-val t-dist dof alpha)
      :smpl-means [smpl-mean-one smpl-mean-two]
      :pop-means [pop-mean-one pop-mean-two]
      :pool-vars [pool-var-one pool-var-two]
      :smpl-sizes [smpl-size-one smpl-size-two]
      :type :TTest)))


(defmethod ttest Welch [this]
  (let [pcalcs (pvalues (map mean (.smpls this))
                        (map #(smpl-var % (mean %)) (.smpls this))
                        (map count (.smpls this)))
        [[mean-one mean-two] [smpl-var-one smpl-var-two] [smpl-size-one smpl-size-two]] pcalcs
          dof (/ (* (+ (/ smpl-var-one smpl-size-one)
                       (/ smpl-var-two smpl-size-two))
                    (+ (/ smpl-var-one smpl-size-one)
                       (/ smpl-var-two smpl-size-two)))
                 (+ (/ (* (/ smpl-var-one smpl-size-one)
                          (/ smpl-var-one smpl-size-one))
                       (- smpl-size-one 1))
                    (/ (* (/ smpl-var-two smpl-size-two)
                          (/ smpl-var-two smpl-size-two))
                       (- smpl-size-two 1))))
          alpha (.alpha this)]
    (assoc {}
      :smpls (.smpls this)
      :t-stat (/ (- mean-one mean-two)
                 (Math/sqrt (+ (/ smpl-var-one smpl-size-one)
                               (/ smpl-var-two smpl-size-two))))
      :dof dof
      :alpha alpha
      :crtcl-val (crtcl-val t-dist (Math/round dof) alpha)
      :smpl-means [mean-one mean-two]
      :smpl-vars [smpl-var-one smpl-var-two]
      :smpl-sizes [smpl-size-one smpl-size-two]
      :type :TTest)))


(defmethod ttest RepeatedMeasure [this]
  (let [pcalcs (pvalues (mean (difference (.smpls this)))
                        (map mean (partition 1 (.h-means this)))
                        (smpl-std-dev (difference (.smpls this)) (mean (difference (.smpls this))))
                        (/ (+ (count (first (.smpls this))) (count (second (.smpls this)))) 2))
        [diff-mean [pop-mean-one pop-mean-two] std-dev smpl-size] pcalcs
         dof (dec smpl-size)
         alpha (.alpha this)]
    (assoc {}
      :smpls (.smpls this)
      :h-means (.h-means this)
      :t-stat (/ (- diff-mean
                    (- pop-mean-one pop-mean-two))
                 (/ std-dev
                    (Math/sqrt smpl-size)))
      :dof dof
      :alpha alpha
      :crtcl-val (crtcl-val t-dist dof alpha)
      :pop-means [pop-mean-one pop-mean-two]
      :std-dev std-dev
      :smpl-size smpl-size
      :diff-mean diff-mean
      :type :TTest)))





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
