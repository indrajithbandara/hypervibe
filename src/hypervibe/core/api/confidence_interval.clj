(ns hypervibe.core.api.confidence_interval
	(:require
		[clojure.core.matrix :as m]
		[hypervibe.core.api.utils :refer [mean diff ssdev svar pvar]]))

(deftype OneSample [smpl cval hmean])
(deftype EqualVariance [smpls cval])

(deftype -OneSample [smpl cval hmean])

(defmulti cintvl class)

(defn one-smpl-conf-int
	[smpl cval hmean
	 ssdev smean ssize
	 mdiff upper lower]
	^{:type :OneSample}
	{:smpl  smpl
	 :cval  cval
	 :hmean hmean
	 :ssdev ssdev
	 :smean smean
	 :ssize ssize
	 :mdiff mdiff
	 :upper upper
	 :lower lower})

(defn two-smpl-conf-int
	[smpls svars smeans
	 ssizes cval upper lower]
	^{:type :EqualVariance}
	{:smpls  smpls
	 :svars  svars
	 :smeans smeans
	 :ssizes ssizes
	 :cval   cval
	 :upper  upper
	 :lower  lower})

(defmethod cintvl OneSample [this]
	(let [[smean ssdev ssize]
		  (pvalues (mean (.smpl this))
				   (ssdev (.smpl this))
				   (m/ecount (.smpl this)))
		  mdiff (- smean (.hmean this))]
		(one-smpl-conf-int (.smpl this)
						   (.cval this)
						   (.hmean this)
						   ssdev
						   smean
						   ssize
						   mdiff (+ mdiff
									(* (.cval this)
									   (/ ssdev
										  (Math/sqrt ssize))))
						   (- mdiff
							  (* (.cval this)
								 (/ ssdev
									(Math/sqrt ssize)))))))

(defmethod cintvl EqualVariance [this]
	(let [[[smean-one smean-two]
		   [svar-one svar-two]
		   [ssize-one ssize-two]]
		  (pvalues (map mean
						(.smpls this))
				   (map #(svar % (mean %))
						(.smpls this))
				   (map count
						(.smpls this)))]
		(two-smpl-conf-int
			(.smpls this)
			[svar-one svar-two]
			[smean-one smean-two]
			[ssize-one smean-two]
			(.cval this)
			(+ (- smean-one
				  smean-two)
			   (* (.cval this)
				  (Math/sqrt (+ (/ svar-one
								   ssize-one)
								(/ svar-two
								   ssize-two)))))
			(- (- smean-one
				  smean-two)
			   (* (.cval this)
				  (Math/sqrt (+ (/ svar-one
								   ssize-one)
								(/ svar-two
								   ssize-two))))))))


;(defmethod cintvl Welch [this])
;(defmethod cintvl RepeatedMeasure [this])
