
(ns clojure.core.stats.estimate.confidence_interval
  (:require [clojure.stats.estimate.confidence_interval :refer [confidence-interval]])
  (:import [clojure.stats.estimate.confidence_interval OneSample EqualVariance]))
(use 'criterium.core)

(defn one-smpl-conf-intvl
  "TODO"
  [{smpl :smpl crtcl-val :cval hmean :hmean}]
  (confidence-interval (OneSample. smpl crtcl-val hmean)))


(defn equal-var-conf-intvl
  "TODO"
  [{smpls :smpls crtcl-val :crtcl-val}]
  (confidence-interval (EqualVariance. smpls crtcl-val)))

