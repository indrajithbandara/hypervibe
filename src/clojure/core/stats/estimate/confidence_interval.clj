
(ns clojure.core.stats.estimate.confidence_interval
  (:require [clojure.stats.estimate.confidence_interval :refer [cintvl]])
  (:import [clojure.stats.estimate.confidence_interval OneSample EqualVariance]))
(use 'criterium.core)

(defn osc-intvl
  "TODO"
  [{smpl :smpl cval :cval hmean :hmean}]
  (cintvl (OneSample. smpl cval hmean)))


(defn evc-intvl
  "TODO"
  [{smpls :smpls cval :cval}]
  (cintvl (EqualVariance. smpls cval)))

