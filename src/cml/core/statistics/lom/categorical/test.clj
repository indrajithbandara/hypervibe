(ns cml.core.statistics.lom.categorical.test
  (:require [cml.utils.probability.functions :refer [expected-values]]
            [cml.statistics.lom.categorical.test :refer [pearson-chi-square]])
  (:import [cml.statistics.lom.categorical.test Independance]))

(defn chi-square-test [{:keys [observed nrows ncols]}]
  (pearson-chi-square (Independance. observed nrows ncols)))

