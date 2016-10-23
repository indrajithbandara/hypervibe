(ns clojure.stats.estimate
  (:require [clojure.utils.variation :refer [smpl-std-dev]]
            [clojure.utils.central-tendancy :refer [mean]]))

;TODO parallel versions as in interval api

(defprotocol Estimate
  (confidence-interval [ci] "Confidence imterval"))

(defrecord OneSample [sample critical-value]
  Estimate
  (confidence-interval [type]
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


;TODO parallel version. Move computations in this namespace as in interval ns
(defrecord TwoSample [sample-mean sample-variance sample-size critical-value]
  Estimate

  (confidence-interval [type]
    (let [[sample-mean-one sample-mean-two] sample-mean
          [sample-variance-one sample-variance-two] sample-variance
          [sample-size-one sample-size-two] sample-size]
      (assoc type
        :upper (+ (- sample-mean-one sample-mean-two)
                  (* critical-value
                     (Math/sqrt (+ (/ sample-variance-one sample-size-one)
                                   (/ sample-variance-two sample-size-two)))))
        :lower (- (- sample-mean-one sample-mean-two)
                  (* critical-value
                     (Math/sqrt (+ (/ sample-variance-one sample-size-one)
                                   (/ sample-variance-two sample-size-two)))))))))

