(ns clojure.stats.estimate)

;TODO parallel versions as in interval api

(defprotocol Estimate
  (confidence-interval [ci] "Confidence imterval"))

;TODO parallel version. Move computations in this namespace as in interval ns
(defrecord OneSample [sample-mean sample-standard-deviation sample-size critical-value]
  Estimate

  (confidence-interval [type]
    (assoc type
      :upper (+ sample-mean
                (* critical-value
                   (/ sample-standard-deviation
                      (Math/sqrt sample-size))))
      :lower (- sample-mean
                (* critical-value
                   (/ sample-standard-deviation
                      (Math/sqrt sample-size)))))))


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


