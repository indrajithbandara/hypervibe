(ns clojure.stats.estimate.confidence-interval
  (:require [clojure.stats.test :refer [one-sample equal-variance]]
            [clojure.stats.utils.central-tendancy :refer [mean difference]]
            [clojure.stats.utils.variation :refer [smpl-std-dev smpl-var pool-var]])
  (:import [clojure.stats.test TTest]))


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

