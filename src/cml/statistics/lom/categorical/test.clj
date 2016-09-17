(ns cml.statistics.lom.categorical.test
  (:require [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal]
            [uncomplicate.commons.core :refer [with-release]])
  (:use [uncomplicate.fluokitten core jvm]))

(defprotocol Categorical
  (pearson-chi-square [this]
    "Conducts a pearson chi square test on a dataset
     that has acategorical level of measurement"))

(defrecord Independance [observed nrows ncols]
  Categorical
  (pearson-chi-square [type]
    (with-release [mtrx (neanderthal-native/dge nrows ncols observed)
                   one-v-rows (neanderthal/entry! (neanderthal-native/dv nrows) 1.0)
                   zero-v-cols (neanderthal-native/dv ncols)
                   transposed-mtrx (neanderthal/trans mtrx)]
                  (assoc type :chi (neanderthal/sum (neanderthal/mv!
                                       (fmap! (^:once fn* ^double [^double x ^double y]
                                                (/ (* (- x y)
                                                      (- x y)) y)) transposed-mtrx
                                              (fmap! (fn ^double [^double x]
                                                       (/ x (neanderthal/sum (neanderthal-native/dv observed))))
                                                     (neanderthal/rank
                                                       (neanderthal/mv! transposed-mtrx one-v-rows zero-v-cols)
                                                       (neanderthal/mv! mtrx (neanderthal/entry! (neanderthal-native/dv ncols) 1.0)
                                                                        (neanderthal-native/dv nrows)))))
                                       one-v-rows zero-v-cols))))))


