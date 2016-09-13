(ns cml.statistics.categorical.test
  (:require [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal])
  (:use [uncomplicate.fluokitten core jvm]))

(defprotocol Categorical
  (pearson-chi-square [s] "Conducts a Chi Square test on a categorical data set"))


;TODO see if fmap has something to do with out of memory
(defrecord Independance [observed nrows ncols]
  Categorical
  (pearson-chi-square [type]
    (let [mtrx (neanderthal-native/dge nrows ncols observed)
          one-v (neanderthal/entry! (neanderthal-native/dv nrows) 1.0)
          zero-v (neanderthal-native/dv ncols)
          sum-obs (neanderthal/sum (neanderthal-native/dv observed))
          transposed-mtrx (neanderthal/trans mtrx)]
      (assoc type :chi (neanderthal/sum (neanderthal/mv!
                                          (fmap! (fn ^double [^double x ^double y]
                                                   (/ (* (- x y)
                                                         (- x y)) y)) transposed-mtrx
                                                 (fmap! (fn ^double [^double x] (/ x sum-obs))
                                                        (neanderthal/rank
                                                          (neanderthal/mv! transposed-mtrx one-v zero-v)
                                                          (neanderthal/mv! mtrx one-v (neanderthal-native/dv ncols))))) one-v zero-v))))))

