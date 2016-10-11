(ns cs.statistics.lom.categorical.test
  (:require [uncomplicate.neanderthal.native :as nn]
            [uncomplicate.neanderthal.core :as n]
            [uncomplicate.commons.core :refer [with-release]]
            [cs.utils.vector :refer [vminus]])
  (:use [uncomplicate.fluokitten core jvm]))
(use 'criterium.core)

(defprotocol Categorical
  (pearson-chi-square [this]
    "Conducts a pearson chi square test on a dataset
     that has acategorical level of measurement"))

;TODO re implement have BLAS functionality in functions only to stop complexity for minimal perf gain
(defrecord Independance [data nrows ncols]
  Categorical
  (pearson-chi-square [type]))
