(ns clojure.stats.distribution.t.critical-value
  (:require [clojure.stats.distribution.t.table :refer [t one-tail-alpha-value two-tail-alpha-value]]))
(use 'clojure.core.matrix)


;TODO change to protocols

(defn one-tail [{:keys [dof alpha]}]
  {:critical-value
   (mget t (dec dof) ((one-tail-alpha-value) alpha))
   :dof dof
   :alpha alpha})



(defn two-tail [{:keys [dof alpha]}]
  {:critical-value
          (mget t (dec dof)
                ((two-tail-alpha-value) alpha))
   :dof   dof
   :alpha alpha})