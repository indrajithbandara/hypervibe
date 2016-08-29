(ns cml.core.data.specifications)
;
;
;(s/def ::sample (s/and not-empty (s/coll-of number?)))
;(s/def ::critical-value number?)
;(s/def ::one-sample-conf-inter (s/keys :req [::sample ::critical-value]))
;
;
;(defmulti data :spec)
;(defmulti explain :spec)
;
;
;(defmethod explain :one-sample-conf-inter [{:keys [sample critical-value]}]
;  (s/explain ::one-sample-conf-inter {::sample sample ::critical-value critical-value}))
;
;
;(defmethod data :one-sample-conf-inter [{:keys [sample critical-value]}]
;  (if (s/valid? ::one-sample-conf-inter {::sample sample ::critical-value critical-value})
;    {:sample sample :critical-value critical-value}
;    (assert (explain {:sample sample :critical-value critical-value :spec :one-sample-conf-inter}))))


