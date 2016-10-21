(ns clojure.stats.lom.categorical.test
  (:require [clojure.utils.vector :refer [vminus]]))

(defprotocol Categorical
  (pearson-chi-square [this]
    "Conducts a pearson chi square test on a dataset
     that has acategorical level of measurement"))

;TODO re implement have BLAS functionality in functions only to stop complexity for minimal perf gain
(defrecord Independance [data nrows ncols]
  Categorical
  (pearson-chi-square [type]))
