(ns clojure.stats.estimate.confidence_interval
  (:require [clojure.stats.utils :refer [mean diff ssdev svar pvar]]))

(deftype OneSample [smpl cval hmean])
(deftype EqualVariance [smpls cval])

(defmulti cintvl class)

(defn one-smpl [])
(defn equal-var [])

(defn posmpl [this]
  (pvalues (mean (.smpl this))
           (ssdev (.smpl this) (mean (.smpl this)))
           (count (.smpl this))))

(defmethod cintvl OneSample [this]
  (let [[smean ssdev ssize] (posmpl this)
         mdiff (- smean (.hmean this))]
    (assoc {}
      :smpl (.smpl this)
      :cval  (.cval this)
      :hmean (.hmean this)
      :ssdev ssdev
      :smean smean
      :ssize ssize
      :mdiff mdiff
      :upper (+ mdiff
                (*  (.cval this) (/ ssdev (Math/sqrt ssize))))
      :lower (- mdiff
                (*  (.cval this)
                   (/ ssdev
                      (Math/sqrt ssize))))
      :type :ConfidenceInterval)))


(defmethod cintvl EqualVariance [this]
  (let [pcalcs (pvalues (map mean (.smpls this))
                        (map #(svar % (mean %)) (.smpls this))
                        (map count (.smpls this)))
        [[smean-one smean-two] [svar-one svar-two] [ssize-one ssize-two]] pcalcs]
    (assoc {}
      :smpls (.smpls this)
      :svars [svar-one svar-two]
      :smeans [smean-one smean-two]
      :ssizes [ssize-one smean-two]
      :cval (.cval this)
      :upper (+ (- smean-one smean-two)
                (* (.cval this)
                   (Math/sqrt (+ (/ svar-one ssize-one)
                                 (/ svar-two ssize-two)))))
      :lower (- (- smean-one smean-two)
                (* (.cval this)
                   (Math/sqrt (+ (/ svar-one ssize-one)
                                 (/ svar-two ssize-two)))))
      :type :ConfidenceInterval)))


;(defmethod cintvl Welch [this])
;(defmethod cintvl RepeatedMeasure [this])
