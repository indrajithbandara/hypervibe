(ns cml.core.statistics.categorical.test
  (:require [cml.utils.probability.functions :refer [expected-values]])
  (:import [cml.statistics.categorical.test Independance]))


(defn chi-square-test [{:keys [observed]}]
  (chi-square-test (Independance. observed (expected-values observed))))

