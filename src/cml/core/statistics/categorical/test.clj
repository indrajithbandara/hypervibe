(ns cml.core.statistics.categorical.test
  (:require [cml.utils.probability.functions :refer [expected-values]]
            [cml.statistics.categorical.test :refer [pearson-chi-square]])
  (:import [cml.statistics.categorical.test Independance]))
(use 'criterium.core)

(defn chi-square-test [{:keys [observed nrows ncols]}]
  (pearson-chi-square (Independance. observed nrows ncols)))

