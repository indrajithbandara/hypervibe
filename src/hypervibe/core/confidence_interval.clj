
(ns hypervibe.core.confidence_interval
  (:require [hypervibe.core.api.confidence_interval :refer [cintvl]])
  (:import [hypervibe.core.api.confidence_interval OneSample EqualVariance]))
(use 'criterium.core)

(defn osc-intvl
  "TODO"
  [{smpl :smpl cval :cval hmean :hmean}]
  (cintvl (OneSample. smpl cval hmean)))


(defn evc-intvl
  "TODO"
  [{smpls :smpls cval :cval}]
  (cintvl (EqualVariance. smpls cval)))

