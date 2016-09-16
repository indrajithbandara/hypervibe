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


(defrecord -Independance [observed nrows ncols]
  Categorical
  (pearson-chi-square [type]
    (with-release [mtrx (neanderthal-native/dge nrows ncols observed)
                   one-v (neanderthal/entry! (neanderthal-native/dv ncols) 1.0)
                   one-v-rows (neanderthal/entry! (neanderthal-native/dv nrows) 1.0)
                   zero-v-cols (neanderthal-native/dv ncols)
                   zero-v (neanderthal-native/dv nrows)
                   transposed-mtrx (neanderthal/trans mtrx)]
                  (assoc type :chi (neanderthal/sum (neanderthal/mv!
                                                      (fmap! (^:once fn* ^double [^double x ^double y]
                                                               (/ (* (- x y)
                                                                     (- x y)) y)) transposed-mtrx
                                                             (fmap! (fn ^double [^double x]
                                                                      (/ x (neanderthal/sum (neanderthal-native/dv observed))))
                                                                    (neanderthal/rank
                                                                      (neanderthal/mv! transposed-mtrx one-v-rows zero-v-cols)
                                                                      (neanderthal/mv! mtrx one-v (neanderthal-native/dv nrows)))))
                                                      one-v-rows zero-v-cols))))))


(time (pearson-chi-square (Independance. [1 2 3 4 5 6 7 8] 4 2)))

(neanderthal/mv! (neanderthal-native/dge 4 2 [1 2 3 4 5 6 7 8])
                 (neanderthal-native/dv [1 1])
                 (neanderthal-native/dv [0 0 0 0]))

(neanderthal/mv! (neanderthal/trans (neanderthal-native/dge 4 2 [1 2 3 4 5 6 7 8]))
                 (neanderthal-native/dv [1 1 1 1])
                 (neanderthal-native/dv [0 0]))