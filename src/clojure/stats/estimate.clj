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
          [smpl-mean smpl-std-dev smpl-size crtcl-val] pcalcs]
      (assoc type :smpl-std-dev smpl-std-dev
                  :smpl-mean smpl-mean
                  :smpl-size smpl-size
                  :crtcl-val crtcl-val
                  :upper (+ smpl-mean
                            (* crtcl-val
                               (/ smpl-std-dev
                                  (Math/sqrt smpl-size))))
                  :lower (- smpl-mean
                            (* crtcl-val
                               (/ smpl-std-dev
                                  (Math/sqrt smpl-size))))))))


(defrecord TwoSample [smpls crtcl-val]
  Estimate
  (conf-int [type]
    (let [pcalcs (pvalues (map mean smpls)
                          (map #(smpl-var % (mean %)) smpls)
                          (map count smpls))
          [[smpl-mean-one smpl-mean-two] [smpl-var-one smpl-var-two] [smpl-size-one smpl-size-two]] pcalcs]
      (assoc type :smpl-vars [smpl-var-one smpl-var-two]
                  :smpl-means [smpl-mean-one smpl-mean-two]
                  :smpl-sizes [smpl-size-one smpl-mean-two]
                  :crtcl-val crtcl-val
                  :upper (+ (- smpl-mean-one smpl-mean-two)
                            (* crtcl-val
                               (Math/sqrt (+ (/ smpl-var-one smpl-size-one)
                                             (/ smpl-var-two smpl-size-two)))))
                  :lower (- (- smpl-mean-one smpl-mean-two)
                            (* crtcl-val
                               (Math/sqrt (+ (/ smpl-var-one smpl-size-one)
                                             (/ smpl-var-two smpl-size-two)))))))))

