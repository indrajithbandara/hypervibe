(ns clojure.core.stats.estimate
  (:require [clojure.utils.variation :refer [standard-deviation variance]]
            [clojure.stats.estimate :refer [confidence-interval]]
            [clojure.utils.central-tendancy :refer [mean]]
            [clojure.utils.variation :refer [smpl-std-dev smpl-var]])
  (:import [clojure.stats.estimate OneSample TwoSample]))
(use 'criterium.core)

(defn one-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (OneSample. sample critical-value)))

;(s/fdef one-sample-conf-inter
;        :args {:sample         (s/and not-empty (s/coll-of int?))
;               :critical-value number?}
;        :ret map?)


(defn two-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (TwoSample. sample critical-value)))

;(s/fdef two-sample-conf-inter
;        :args {:sample         (or [sequential? sequential?] '(sequential? sequential?))
;               :critical-value number?}
;        :ret map?)


