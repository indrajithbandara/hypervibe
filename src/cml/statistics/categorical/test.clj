(ns cml.statistics.categorical.test
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.reducers :as r]
            [clojure.core.matrix.operators :as op]
            [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal])
  (:use [uncomplicate.fluokitten core jvm]))


(def observed-vals [60 300 10 390])
(def expected-vals '(33.1578947368421 36.8421052631579 326.8421052631579 363.1578947368421))

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
          expected (for [row-total (map #(neanderthal/sum (neanderthal-native/dv %))
                                        (neanderthal/rows mtrx))
                         column-total (neanderthal/mv! (neanderthal/trans mtrx) one-v zero-v)]
                     (/ (* row-total column-total)
                        (neanderthal/sum (neanderthal-native/dv observed))))]
      (assoc type :chi (neanderthal/sum (neanderthal/mv!
                                          (fmap (fn ^double [^double x ^double y]
                                                  (/ (* (- x y)
                                                        (- x y)) y))
                                                (neanderthal/trans mtrx)
                                                (neanderthal-native/dge nrows ncols expected))
                                          one-v zero-v))))))

;(neanderthal/mv! (fmap neanderthal/sum (neanderthal-native/dge 2 2 [1 2 3 4]) [1.0 1.0] [0 0]))

(def a (range 100000))

(def mtrx (neanderthal-native/dge 2 2 observed-vals))


(for [x (neanderthal/mv! mtrx (neanderthal-native/dv [1 1]) (neanderthal-native/dv [0 0]))]
  (println x))

;;row
(neanderthal/mv! (neanderthal/trans mtrx) (neanderthal-native/dv [1 1]) (neanderthal-native/dv [0 0]))

;;cols
(neanderthal/mv! mtrx (neanderthal-native/dv [1 1]) (neanderthal-native/dv [0 0]))

(defrecord -Independance [observed nrows ncols]
  Categorical
  (pearson-chi-square [type]
    (let [mtrx (neanderthal-native/dge nrows ncols observed)
          one-v (neanderthal/entry! (neanderthal-native/dv nrows) 1.0)
          zero-v (neanderthal-native/dv ncols)
          transpose (neanderthal/trans mtrx)
          expected (for [row-total (neanderthal/mv! transpose one-v zero-v)
                         column-total (neanderthal/mv! mtrx one-v zero-v)]
                     (/ (* row-total column-total)          ;TODO outer product§
                        (neanderthal/sum (neanderthal-native/dv observed))))] ;TODO in process of getting rid of for loop
      (assoc type :chi (neanderthal/sum (neanderthal/mv!
                                          (fmap (fn ^double [^double x ^double y]
                                                  (/ (* (- x y)
                                                        (- x y)) y))
                                                transpose
                                                (neanderthal-native/dge nrows ncols expected))
                                          one-v zero-v))))))


(def row-total (neanderthal/mv! (neanderthal/trans mtrx)
                                (neanderthal-native/dv [1 1])
                                (neanderthal-native/dv [0 0])))

(def column-total (neanderthal/mv! mtrx (neanderthal-native/dv [1 1])
                                   (neanderthal-native/dv [0 0])))
(def sum-obvs (neanderthal/sum (neanderthal-native/dv observed-vals)))

(def rt [360 400])
(def ct [70  690])



(def rt-times-ct '(25200.0 248400.0 28000.0 276000.0))

;rt =>  #RealBlockVector[double, n:2, stride:1](360.0 400.0)
;ct =>  #RealBlockVector[double, n:2, stride:1](70.0 690.0)


(def foo (neanderthal-native/dge 2 2 [60 300 10 390]))


(neanderthal/rank
  column-total
  row-total)


