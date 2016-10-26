(ns clojure.core.stats.estimate
  (:require [clojure.stats.estimate :refer [conf-int]]
            [clojure.stats.utils.central-tendancy :refer [mean]]
            [clojure.stats.utils.variation :refer [smpl-std-dev smpl-var]])
  (:import [clojure.stats.estimate OneSample TwoSample]))
(use 'criterium.core)

(defn one-smpl-conf-inter [{:keys [sample critical-value]}]
  (conf-int (OneSample. sample critical-value)))

;(s/fdef one-sample-conf-inter
;        :args {:sample         (s/and not-empty (s/coll-of int?))
;               :critical-value number?}
;        :ret map?)


(defn two-smpl-conf-inter [{:keys [sample critical-value]}]
  (conf-int (TwoSample. sample critical-value)))

;(s/fdef two-sample-conf-inter
;        :args {:sample         (or [sequential? sequential?] '(sequential? sequential?))
;               :critical-value number?}
;        :ret map?)


