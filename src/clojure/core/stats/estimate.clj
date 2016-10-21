(ns clojure.core.stats.estimate
  (:require [clojure.utils.variation :refer [standard-deviation variance]]
            [clojure.stats.estimate :refer [confidence-interval]]
            [clojure.utils.central-tendancy :refer [mean]]
            )
  (:import [clojure.utils.variation Sample]
           [clojure.stats.estimate OneSample TwoSample]))
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


