(ns clojure.stats.estimate.confidence-interval
  (:require [clojure.stats.utils.central-tendancy :refer [mean difference]]
            [clojure.stats.utils.variation :refer [smpl-std-dev smpl-var pool-var]])
  (:import [clojure.stats.test TTest]))


(defrecord ConfidenceInterval [in]
  TTest

  (one-sample [type]
    (let [pcalcs (pvalues (mean (:smpl in))
                          (smpl-std-dev (:smpl in) (mean (:smpl in)))
                          (count (:smpl in))
                          (:crtcl-val in)
                          (:h-mean in))
          [smpl-mean smpl-std-dev smpl-size crtcl-val h-mean] pcalcs
           mean-diff (- smpl-mean h-mean)]
      (assoc type :smpl-std-dev smpl-std-dev
                  :smpl-mean smpl-mean
                  :smpl-size smpl-size
                  :mean-diff mean-diff
                  :upper (+ mean-diff
                                   (* crtcl-val (/ smpl-std-dev (Math/sqrt smpl-size))))
                  :lower (- mean-diff
                                   (* crtcl-val
                                      (/ smpl-std-dev
                                         (Math/sqrt smpl-size)))))))

  (equal-variance [type]
    (let [pcalcs (pvalues (map mean (:smpls in))
                          (map #(smpl-var % (mean %)) (:smpls in))
                          (map count (:smpls in)))
          [[smpl-mean-one smpl-mean-two] [smpl-var-one smpl-var-two] [smpl-size-one smpl-size-two]] pcalcs]
      (assoc type :smpl-vars [smpl-var-one smpl-var-two]
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
                                                    (/ smpl-var-two smpl-size-two))))))))
  (welch [type] (println type))

  (repeated-measure [type] ""))

; 2.2621571627982 two tail (0.05 / 2)