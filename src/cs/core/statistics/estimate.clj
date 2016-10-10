(ns cml.core.statistics.estimate
  (:require [cml.utils.variation :refer [standard-deviation variance]]
            [cml.statistics.estimate :refer [confidence-interval]]
            [cml.utils.central-tendancy :refer [mean]]
            )
  (:import [cml.utils.variation Sample]
           [cml.statistics.estimate OneSample TwoSample]))
(use 'criterium.core)

(defn one-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (OneSample. (mean sample)
                                   (:standard-deviation (standard-deviation (Sample. (mean sample) sample)))
                                   (count sample) critical-value)))

;(s/fdef one-sample-conf-inter
;        :args {:sample         (s/and not-empty (s/coll-of int?))
;               :critical-value number?}
;        :ret map?)


(defn two-sample-conf-inter [{:keys [sample critical-value]}]
  (confidence-interval (TwoSample. (map mean sample)
                                   (map #(:variance (variance (Sample. (mean %) %))) sample)
                                   (map count sample) critical-value)))

;(s/fdef two-sample-conf-inter
;        :args {:sample         (or [sequential? sequential?] '(sequential? sequential?))
;               :critical-value number?}
;        :ret map?)


