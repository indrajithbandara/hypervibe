
(ns clojure.core.stats.estimate.confidence-interval
  (:require [clojure.stats.test :refer [one-sample equal-variance]])
  (:import [clojure.stats.estimate.confidence_interval ConfidenceInterval]))
(use 'criterium.core)

(defn one-smpl-conf-intvl
  "TODO"
  [data] (one-sample (ConfidenceInterval. data)))


(defn equal-var-conf-intvl
  "TODO"
  [data] (equal-variance (ConfidenceInterval. data)))

