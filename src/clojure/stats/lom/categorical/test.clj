(ns clojure.stats.lom.categorical.test
  (:require [clojure.utils.vector :refer [vminus]]))

(defprotocol Categorical
  (pearson-chi-square [this]
    "Conducts a pearson chi square test on a dataset
     that has acategorical level of measurement"))

;TODO implement next
(defrecord Independance [data nrows ncols]
  Categorical
  (pearson-chi-square [type]))
