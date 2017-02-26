
(ns clojure.core.stats.confidence_interval
  (:require [clojure.stats.confidence_interval :refer [cintvl]])
  (:import [clojure.stats.confidence_interval OneSample EqualVariance]))
(use 'criterium.core)

(defn osc-intvl
  "TODO"
  [{smpl :smpl cval :cval hmean :hmean}]
  (cintvl (OneSample. smpl cval hmean)))


(defn evc-intvl
  "TODO"
  [{smpls :smpls cval :cval}]
  (cintvl (EqualVariance. smpls cval)))

