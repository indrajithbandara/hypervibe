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


(defrecord Test [in]
  TTest

  (one-sample [type]
    (let [pcalcs (pvalues (mean (:smpl in))
                          (smpl-std-dev (:smpl in) (mean (:smpl in)))
                          (count (:smpl in)))
          [smpl-mean smpl-std-dev smpl-size] pcalcs
          dof (dec smpl-size)
          alpha (or (:alpha in) 0.05)]
      (assoc type
        :out { :t-stat (/ (- smpl-mean (or (:h-mean in) 0))
                          (/ smpl-std-dev
                             (Math/sqrt smpl-size)))
              :dof dof
              :alpha alpha
              :crtcl-val (crtcl-val t-dist dof alpha)
              :smpl-mean smpl-mean
              :smpl-std-dev smpl-std-dev
              :smpl-size smpl-size})))


  (equal-variance [type]
    (let [pcalcs (pvalues (map mean (:smpls in))
                          (map mean (partition 1 (or (:h-means in) [0 0])))
                          (map #(pool-var % (mean %) (dec (count %))) (:smpls in))
                          (map count (:smpls in)))
          [[smpl-mean-one smpl-mean-two] [pop-mean-one pop-mean-two] [pool-var-one pool-var-two]
           [smpl-size-one smpl-size-two]] pcalcs
          dof (- (+ smpl-size-one smpl-size-two) 2)
          alpha (or (:alpha in) 0.05)]
      (assoc type :out { :t-stat (/ (- (- smpl-mean-one smpl-mean-two)
                                       (- pop-mean-one pop-mean-two))
                                    (Math/sqrt (* (/ (+ pool-var-one pool-var-two) 2)
                                                  (+ (/ 1 smpl-size-one) (/ 1 smpl-size-two)))))
                        :dof dof
                        :alpha alpha
                        :crtcl-val (crtcl-val t-dist dof alpha)
                        :smpl-means [smpl-mean-one smpl-mean-two]
                        :pop-means [pop-mean-one pop-mean-two]
                        :pool-vars [pool-var-one pool-var-two]
                        :smpl-sizes [smpl-size-one smpl-size-two]})))

  (welch [type]
    (let [pcalcs (pvalues (map mean (:smpls in))
                          (map #(smpl-var % (mean %)) (:smpls in))
                          (map count (:smpls in)))
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
          alpha (or (:alpha in) 0.05)]
      (assoc type :out {:t-stat (/ (- mean-one mean-two)
                                   (Math/sqrt (+ (/ smpl-var-one smpl-size-one)
                                                 (/ smpl-var-two smpl-size-two))))
                        :dof dof
                        :alpha alpha
                        :crtcl-val (crtcl-val t-dist (Math/round dof) alpha)
                        :smpl-means [mean-one mean-two]
                        :smpl-vars [smpl-var-one smpl-var-two]
                        :smpl-sizes [smpl-size-one smpl-size-two]})))

  (repeated-measure [type]
    (let [pcalcs (pvalues (mean (difference (:smpls in)))
                          (map mean (partition 1 (or (:h-means in) [0 0])))
                          (smpl-std-dev (difference (:smpls in)) (mean (difference (:smpls in))))
                          (/ (+ (count (first (:smpls in))) (count (second (:smpls in)))) 2))
          [diff-mean [pop-mean-one pop-mean-two] std-dev smpl-size] pcalcs
          dof (dec smpl-size)
          alpha (or (:alpha in) 0.05)]
      (assoc type :out {:t-stat (/ (- diff-mean
                                      (- pop-mean-one pop-mean-two))
                                   (/ std-dev
                                      (Math/sqrt smpl-size)))
                        :dof dof
                        :alpha alpha
                        :crtcl-val (crtcl-val t-dist dof alpha)
                        :pop-means [pop-mean-one pop-mean-two]
                        :std-dev std-dev
                        :smpl-size smpl-size
                        :diff-mean diff-mean})))
  (median [type] (println type)))



(defrecord ConfidenceInterval [in]
  TTest

  (one-sample [type]
    (let [pcalcs (pvalues (mean (:smpl in))
                          (smpl-std-dev (:smpl in) (mean (:smpl in)))
                          (count (:smpl in))
                          (:crtcl-val in))
          [smpl-mean smpl-std-dev smpl-size crtcl-val] pcalcs]
      (assoc type :out {:smpl-std-dev smpl-std-dev
                        :smpl-mean smpl-mean
                        :smpl-size smpl-size
                        :crtcl-val crtcl-val
                        :upper (+ smpl-mean
                                  (* crtcl-val
                                     (/ smpl-std-dev
                                        (Math/sqrt smpl-size))))
                        :lower (- smpl-mean
                                  (* (:crtcl-val in)
                                     (/ smpl-std-dev
                                        (Math/sqrt smpl-size))))})))

  (equal-variance [type]
    (let [pcalcs (pvalues (map mean (:smpls in))
                          (map #(smpl-var % (mean %)) (:smpls in))
                          (map count (:smpls in)))
          [[smpl-mean-one smpl-mean-two] [smpl-var-one smpl-var-two] [smpl-size-one smpl-size-two]] pcalcs]
      (assoc type :out {:smpl-vars  [smpl-var-one smpl-var-two]
                        :smpl-means [smpl-mean-one smpl-mean-two]
                        :smpl-sizes [smpl-size-one smpl-mean-two]
                        :crtcl-val (:crtcl-val in)
                        :upper (+ (- smpl-mean-one smpl-mean-two)
                                  (* (:crtcl-val in)
                                     (Math/sqrt (+ (/ smpl-var-one smpl-size-one)
                                                   (/ smpl-var-two smpl-size-two)))))
                        :lower (- (- smpl-mean-one smpl-mean-two)
                                  (* (:crtcl-val in)
                                     (Math/sqrt (+ (/ smpl-var-one smpl-size-one)
                                                   (/ smpl-var-two smpl-size-two)))))}))))




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


