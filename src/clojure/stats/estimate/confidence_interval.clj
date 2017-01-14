(ns clojure.stats.estimate.confidence_interval
  (:require [clojure.stats.utils :refer [mean difference smpl-std-dev smpl-var pool-var]]))

(deftype OneSample [smpl crtcl-val hmean])
(deftype EqualVariance [smpls crtcl-val])

(defmulti confidence-interval class)


(defmethod confidence-interval OneSample [this]
  (let [pcalcs (pvalues (mean (.smpl this))
                        (smpl-std-dev (.smpl this) (mean (.smpl this)))
                        (count (.smpl this))
                        (.crtcl-val this)
                        (.hmean this))
        [smpl-mean smpl-std-dev smpl-size crtcl-val hmean] pcalcs
         mean-diff (- smpl-mean hmean)]
    (assoc {}
      :smpl (.smpl this)
      :crtcl-val crtcl-val
      :hmean hmean
      :smpl-std-dev smpl-std-dev
      :smpl-mean smpl-mean
      :smpl-size smpl-size
      :mean-diff mean-diff
      :upper (+ mean-diff
                (* crtcl-val (/ smpl-std-dev (Math/sqrt smpl-size))))
      :lower (- mean-diff
                (* crtcl-val
                   (/ smpl-std-dev
                      (Math/sqrt smpl-size))))
      :type :ConfidenceInterval)))


(defmethod confidence-interval EqualVariance [this]
  (let [pcalcs (pvalues (map mean (.smpls this))
                        (map #(smpl-var % (mean %)) (.smpls this))
                        (map count (.smpls this)))
        [[smpl-mean-one smpl-mean-two] [smpl-var-one smpl-var-two] [smpl-size-one smpl-size-two]] pcalcs]
    (assoc {}
      :smpls (.smpls this)
      :smpl-vars [smpl-var-one smpl-var-two]
      :smpl-means [smpl-mean-one smpl-mean-two]
      :smpl-sizes [smpl-size-one smpl-mean-two]
      :crtcl-val (.crtcl-val this)
      :upper (+ (- smpl-mean-one smpl-mean-two)
                (* (.crtcl-val this)
                   (Math/sqrt (+ (/ smpl-var-one smpl-size-one)
                                 (/ smpl-var-two smpl-size-two)))))
      :lower (- (- smpl-mean-one smpl-mean-two)
                (* (.crtcl-val this)
                   (Math/sqrt (+ (/ smpl-var-one smpl-size-one)
                                 (/ smpl-var-two smpl-size-two)))))
      :type :ConfidenceInterval)))


;(defmethod confidence-interval Welch [this])
;(defmethod confidence-interval RepeatedMeasure [this])


; 2.2621571627982 two tail (0.05 / 2)