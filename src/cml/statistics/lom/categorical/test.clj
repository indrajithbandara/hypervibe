(ns cml.statistics.lom.categorical.test
  (:require [uncomplicate.neanderthal.native :as nn]
            [uncomplicate.neanderthal.core :as n]
            [uncomplicate.commons.core :refer [with-release]]
            [cml.utils.vector :refer [vminus]])
  (:use [uncomplicate.fluokitten core jvm]))
(use 'criterium.core)

(defprotocol Categorical
  (pearson-chi-square [this]
    "Conducts a pearson chi square test on a dataset
     that has acategorical level of measurement"))

(defrecord Independance [data nrows ncols]
  Categorical
  (pearson-chi-square [type]
    (with-release [mtrx (nn/dge nrows ncols data)
                   vrows (n/entry! (nn/dv nrows) 1.0)
                   vcols (nn/dv ncols)
                   transposed (n/trans mtrx)]
      (assoc type :chi
        (n/sum (n/mv! (fmap! (^:once fn* ^double [^double x ^double y]
                          (/ (* (- x y) (- x y)) y))
                             transposed
                        (fmap! (fn ^double [^double x]
                                 (/ x (n/sum (nn/dv data))))
                               (n/rank (n/mv! transposed vrows vcols)
                                       (n/mv! mtrx (n/entry! (nn/dv ncols)
                                                             1.0)
                                              (nn/dv nrows)))))
                      vrows vcols))))))
