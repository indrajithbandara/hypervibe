(ns clojure.stats.lom.test
  (:require [clojure.stats.utils.central-tendancy :refer [mean difference]]
            [clojure.stats.utils.variation :refer [smpl-std-dev smpl-var pool-var]]
            [clojure.stats.distribution.t.table :refer [t-dist crtcl-val]]))
(use 'clojure.core.matrix)



(defprotocol TTest
  (one-sample [this] "Conducts a pearson one sample t-test")
  (equal-variance [this] "Conducts an equal variance t-test")
  (welch [this] "Conducts a welch t-test")
  (repeated-measure [this] "Conducts a repeated measure t-test")
  (median [this] "Conducts a median ttest"))

(defrecord Test [data]
  TTest

  (one-sample [type]
    (let [pcalcs (pvalues (mean (:smpl data))
                          (smpl-std-dev (:smpl data) (mean (:smpl data)))
                          (count (:smpl data)))
          [smpl-mean smpl-std-dev smpl-size] pcalcs
          dof (dec smpl-size)]
      (assoc type :t-stat (/ (- smpl-mean (:h-mean data))
                             (/ smpl-std-dev
                                (Math/sqrt smpl-size)))
                  :dof dof
                  :alpha (:alpha data)
                  :crtcl-val (crtcl-val t-dist dof (:alpha data))
                  :smpl-mean smpl-mean
                  :smpl-std-dev smpl-std-dev
                  :smpl-size smpl-size)))

  (equal-variance [type]
    (let [pcalcs (pvalues (map mean (:smpls data))
                          (map mean (partition 1 (:h-means data)))
                          (map #(pool-var % (mean %) (dec (count %))) (:smpls data))
                          (map count (:smpls data)))
          [[smpl-mean-one smpl-mean-two] [pop-mean-one pop-mean-two]
           [pool-var-one pool-var-two] [smpl-size-one smpl-size-two]] pcalcs
          dof (- (+ smpl-size-one smpl-size-two) 2)]
      (assoc type :t-stat (/ (- (- smpl-mean-one smpl-mean-two)
                                (- pop-mean-one pop-mean-two))
                             (Math/sqrt (* (/ (+ pool-var-one pool-var-two) 2)
                                           (+ (/ 1 smpl-size-one) (/ 1 smpl-size-two)))))
                  :dof dof
                  :alpha (:alpha data)
                  :crtcl-val (crtcl-val t-dist dof (:alpha data))
                  :smpl-means [smpl-mean-one smpl-mean-two]
                  :pop-means [pop-mean-one pop-mean-two]
                  :pool-vars [pool-var-one pool-var-two]
                  :smpl-sizes [smpl-size-one smpl-size-two])))

  (welch [type]
    (let [pcalcs (pvalues (map mean (:smpls data))
                          (map #(smpl-var % (mean %)) (:smpls data))
                          (map count (:smpls data)))
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
                  :alpha (:alpha data)
                  :crtcl-val (crtcl-val t-dist (Math/round dof) (:alpha data))
                  :smpl-means [mean-one mean-two]
                  :smpl-vars [smpl-var-one smpl-var-two]
                  :smpl-sizes [smpl-size-one smpl-size-two])))

  (repeated-measure [type]
    (let [pcalcs (pvalues (mean (difference (:smplsdata)))
                          (map mean (partition 1 (:h-means data)))
                          (smpl-std-dev (difference (:smpls data)) (mean (difference (:smpls data))))
                          (/ (+ (count (first (:smpls data))) (count (second (:smpls data)))) 2))
          [diff-mean [pop-mean-one pop-mean-two] std-dev smpl-size] pcalcs
          dof (dec smpl-size)]
      (assoc type :t-stat (/ (- diff-mean
                                (- pop-mean-one pop-mean-two))
                             (/ std-dev
                                (Math/sqrt smpl-size)))
                  :dof dof
                  :alpha (:alpha data)
                  :crtcl-val (crtcl-val t-dist dof (:alpha data))
                  :pop-means [pop-mean-one pop-mean-two]
                  :std-dev std-dev
                  :smpl-size smpl-size
                  :diff-mean diff-mean)))
  (median [type] (println type)))



(defn chi-square
  "Assumes data to be in the form
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



