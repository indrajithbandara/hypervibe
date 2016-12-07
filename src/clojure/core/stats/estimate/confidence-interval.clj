
(ns clojure.core.stats.estimate.confidence-interval
  (:require [clojure.stats.estimate.confidence-interval :refer [confidence-interval]])
  (:import [clojure.stats.estimate.confidence_interval OneSample EqualVariance]))
(use 'criterium.core)

(defn one-smpl-conf-intvl
  "TODO"
  [smpl crtcl-val h-mean] (confidence-interval (OneSample. smpl crtcl-val h-mean)))


(defn equal-var-conf-intvl
  "TODO"
  [smpls crtcl-val] (confidence-interval (EqualVariance. smpls crtcl-val)))

