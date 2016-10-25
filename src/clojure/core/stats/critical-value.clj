(ns clojure.core.stats.critical-value
  (:require [clojure.stats.utils.tables :refer [t-table]]))
(use 'clojure.core.matrix)


(defn one-tail-cv [{:keys [dof alpha]}]
  {:critical-value
          (mget t-table (dec dof)
                ({0.05 0 0.025 1 0.01 2 0.005 3 0.0025 4 0.001 5 0.0005 6}
                  alpha))
   :dof   dof
   :alpha alpha})


(defn two-tail-cv [{:keys [dof alpha]}]
  {:critical-value
          (mget t-table (dec dof)
                ({0.1 0 0.05 1 0.02 2 0.01 3 0.005 4 0.002 5 0.001 6}
                  alpha))
   :dof   dof
   :alpha alpha})

