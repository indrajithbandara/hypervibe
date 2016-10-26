(ns clojure.stats.estimate
  (:require [clojure.stats.utils.variation :refer [smpl-std-dev smpl-var]]
            [clojure.stats.utils.central-tendancy :refer [mean]]))

;TODO parallel versions as in interval api

(defprotocol Estimate
  (conf-int [ci] "Confidence imterval"))

(defrecord OneSample [sample critical-value]
  Estimate
  (conf-int [type]
    (let [pcalcs (pvalues (mean sample)
                          (smpl-std-dev sample (mean sample))
                          (count sample)
                          critical-value)
          [sample-mean sample-standard-deviation sample-size critical-value] pcalcs]
      (assoc type :sample-standard-deviation sample-standard-deviation
                  :sample-mean sample-mean
                  :sample-size sample-size
                  :critical-value critical-value
                  :upper (+ sample-mean
                            (* critical-value
                               (/ sample-standard-deviation
                                  (Math/sqrt sample-size))))
                  :lower (- sample-mean
                            (* critical-value
                               (/ sample-standard-deviation
                                  (Math/sqrt sample-size))))))))


(defrecord TwoSample [samples critical-value]
  Estimate
  (conf-int [type]
    (let [pcalcs (pvalues (map mean samples)
                          (map #(smpl-var % (mean %)) samples)
                          (map count samples))
          [[sample-mean-one sample-mean-two] [sample-variance-one sample-variance-two] [sample-size-one sample-size-two]] pcalcs]
      (assoc type :sample-variances [sample-variance-one sample-variance-two]
                  :sample-means [sample-mean-one sample-mean-two]
                  :sample-sizes [sample-size-one sample-mean-two]
                  :critical-value critical-value
                  :upper (+ (- sample-mean-one sample-mean-two)
                            (* critical-value
                               (Math/sqrt (+ (/ sample-variance-one sample-size-one)
                                             (/ sample-variance-two sample-size-two)))))
                  :lower (- (- sample-mean-one sample-mean-two)
                            (* critical-value
                               (Math/sqrt (+ (/ sample-variance-one sample-size-one)
                                             (/ sample-variance-two sample-size-two)))))))))

