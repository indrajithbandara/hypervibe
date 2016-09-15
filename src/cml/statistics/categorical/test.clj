(ns cml.statistics.categorical.test
  (:require [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal]
            [uncomplicate.commons.core :refer [with-release]])
  (:use [uncomplicate.fluokitten core jvm]))
(use 'criterium.core)

(defprotocol Categorical
  (pearson-chi-square [s] "Conducts a Chi Square test on a categorical data set"))


(defrecord Independance [observed nrows ncols]
  Categorical
  (pearson-chi-square [type]
    (with-release [mtrx (neanderthal-native/dge nrows ncols observed)
                   one-v (neanderthal/entry! (neanderthal-native/dv nrows) 1.0)
                   zero-v (neanderthal-native/dv ncols)
                   transposed-mtrx (neanderthal/trans mtrx)]
      (assoc type :chi (neanderthal/sum (neanderthal/mv!
                                          (fmap! (^:once fn* ^double [^double x ^double y]
                                                   (/ (* (- x y)
                                                         (- x y)) y)) transposed-mtrx
                                                 (fmap! (fn ^double [^double x]
                                                          (/ x (neanderthal/sum (neanderthal-native/dv observed))))
                                                        (neanderthal/rank
                                                          (neanderthal/mv! transposed-mtrx one-v zero-v)
                                                          (neanderthal/mv! mtrx one-v (neanderthal-native/dv ncols)))))
                                          one-v zero-v))))))


