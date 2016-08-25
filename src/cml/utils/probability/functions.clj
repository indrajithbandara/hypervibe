(ns cml.utils.probability.functions
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.reducers :as r]
            [clojure.core.matrix.operators :as op]))


;TODO look at using reducers
(defn expected-values [observed]
  (for [row-total (map #(reduce + %) observed)
        column-total (map #(reduce + %) (matrix/columns observed))]
    (/ (* row-total column-total)
       (double (matrix/esum observed)))))


