(ns clojure.stats.estimate
  (:require [clojure.stats.utils.variation :refer [smpl-std-dev smpl-var]]
            [clojure.stats.utils.central-tendancy :refer [mean]]))


(defprotocol Estimate
  (conf-int [ci] "Confidence imterval"))

(defrecord OneSample [smpl crtcl-val]
  Estimate
  (conf-int [type]
    (let [pcalcs (pvalues (mean smpl)
                          (smpl-std-dev smpl (mean smpl))
                          (count smpl)
                          crtcl-val)
          [sample-mean sample-standard-deviation sample-size critical-value] pcalcs]
      (assoc type :smpl-std-dev sample-standard-deviation
                  :smpl-mean sample-mean
                  :smpl-size sample-size
                  :crtcl-val critical-value
                  :upper (+ sample-mean
                            (* critical-value
                               (/ sample-standard-deviation
                                  (Math/sqrt sample-size))))
                  :lower (- sample-mean
                            (* critical-value
                               (/ sample-standard-deviation
                                  (Math/sqrt sample-size))))))))


(defrecord TwoSample [smpls crtcl-val]
  Estimate
  (conf-int [type]
    (let [pcalcs (pvalues (map mean smpls)
                          (map #(smpl-var % (mean %)) smpls)
                          (map count smpls))
          [[sample-mean-one sample-mean-two] [sample-variance-one sample-variance-two] [sample-size-one sample-size-two]] pcalcs]
      (assoc type :smpl-vars [sample-variance-one sample-variance-two]
                  :smpl-means [sample-mean-one sample-mean-two]
                  :smpl-sizes [sample-size-one sample-mean-two]
                  :crtcl-val crtcl-val
                  :upper (+ (- sample-mean-one sample-mean-two)
                            (* crtcl-val
                               (Math/sqrt (+ (/ sample-variance-one sample-size-one)
                                             (/ sample-variance-two sample-size-two)))))
                  :lower (- (- sample-mean-one sample-mean-two)
                            (* crtcl-val
                               (Math/sqrt (+ (/ sample-variance-one sample-size-one)
                                             (/ sample-variance-two sample-size-two)))))))))

