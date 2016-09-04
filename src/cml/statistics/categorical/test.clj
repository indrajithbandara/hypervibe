(ns cml.statistics.categorical.test
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.reducers :as r]
            [clojure.core.matrix.operators :as op]
            [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal])
  (:use [uncomplicate.fluokitten core jvm]))


(def observed-vals [60 300 10 390])
;TODO swap out map for fmap and look at changing
;TODO move the creation of a matrix outside of the function eg pull mtrx out

(defprotocol Categorical
  (pearson-chi-square [s] "Conducts a Chi Square test on a categorical data set"))

(defrecord Independance [observed nrows ncols]
  Categorical
  (pearson-chi-square [type]
    (let [mtrx (neanderthal-native/dge nrows ncols observed)
          one-v (neanderthal/entry! (neanderthal-native/dv nrows) 1.0)
          zero-v (neanderthal-native/dv ncols)
          expected (for [row-total (map #(neanderthal/sum (neanderthal-native/dv %)) (neanderthal/rows mtrx))
                         column-total (neanderthal/mv! (neanderthal/trans mtrx) one-v zero-v)]
                     (/ (* row-total column-total)
                        (neanderthal/sum (neanderthal-native/dv observed))))]
      (assoc type :chi (neanderthal/sum (neanderthal/mv!
                                          (fmap (fn ^double [^double x ^double y]
                                                  (/ (* (- x y)
                                                        (- x y)) y))
                                                (neanderthal/trans mtrx) (neanderthal-native/dge nrows ncols expected)) one-v zero-v))))))

(neanderthal/mv! (fmap neanderthal/sum (neanderthal-native/dge 2 2 [1 2 3 4]) [1.0 1.0] [0 0]))
