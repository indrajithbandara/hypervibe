(ns cml.utils.probability.functions
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.reducers :as r]
            [clojure.core.matrix.operators :as op]
            [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal]))
(use 'criterium.core)

;TODO look at using reducers



(def obs [[60 300] [10 390]])

(def a (neanderthal-native/dge 2 3 [60 10 300 390 4 5]))

(def x (neanderthal/entry! (neanderthal-native/dv 2) 1.0))

(neanderthal/mv! (neanderthal/trans (neanderthal-native/dge 2 2 [60 10 300 390])) (neanderthal/entry! (neanderthal-native/dv 2) 1.0) (neanderthal-native/dv 2))

(def col-sums (neanderthal-native/dv 3))

(neanderthal/mv! (neanderthal/trans a) x col-sums)

(neanderthal/entry! (neanderthal-native/sv 10000) 1.0)

(matrix/columns [[60 300] [10 390]])

(neanderthal/ncols a)
(neanderthal/mrows a)

(defn -expected-values [matrix nrows ncols]
  (for [row-total (map #(neanderthal/sum (neanderthal-native/dv %)) (neanderthal/rows (neanderthal-native/dge nrows ncols matrix)))
        column-total (neanderthal/mv!
                       (neanderthal/trans (neanderthal-native/dge nrows ncols matrix))
                       (neanderthal/entry! (neanderthal-native/dv nrows) 1.0)
                       (neanderthal-native/dv ncols))]
    (/ (* row-total column-total)
       (reduce + (map #(neanderthal/sum (neanderthal-native/dv %)) (neanderthal-native/dge nrows ncols matrix))))))

(defn sum-cols [matrix nrows ncols]
  (neanderthal/mv! (neanderthal/trans (neanderthal-native/dge nrows ncols matrix))
                   (neanderthal/entry! (neanderthal-native/dv nrows) 1.0) (neanderthal-native/dv ncols)))



