(ns cml.core.statistics.categorical.test
  (:require [cml.utils.probability.functions :refer [expected-values]]
            [cml.statistics.categorical.test :refer [pearson-chi-square]])
  (:import [cml.statistics.categorical.test Independance]))


(defn chi-square-test [{:keys [observed]}]
  (pearson-chi-square (Independance. observed (expected-values observed))))

