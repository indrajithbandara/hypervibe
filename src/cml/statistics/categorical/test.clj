(ns cml.statistics.categorical.test
  (:require [clojure.core.matrix :as matrix]
            [clojure.core.reducers :as r]
            [clojure.core.matrix.operators :as op]
            [uncomplicate.neanderthal.native :as neanderthal-native]
            [uncomplicate.neanderthal.core :as neanderthal])
  (:use [uncomplicate.fluokitten core jvm]))


(defprotocol Categorical
  (pearson-chi-square [s] "Conducts a Chi Square test on a categorical data set"))

(defrecord -Independance [observed nrows ncols]
  Categorical
  (pearson-chi-square [type]
    (let [expected (let [mtrx (neanderthal-native/dge nrows ncols observed)]
                     (for [row-total (map #(neanderthal/sum (neanderthal-native/dv %)) (neanderthal/rows mtrx))
                           column-total (neanderthal/mv! (neanderthal/trans mtrx)
                                                         (neanderthal/entry! (neanderthal-native/dv nrows) 1.0)
                                                         (neanderthal-native/dv ncols))]
                       (/ (* row-total column-total)
                          (neanderthal/sum (neanderthal-native/dv observed)))))]

      (assoc type :chi (reduce + (map (fn [obs exp]
                                        (/ (* (- obs exp)
                                              (- obs exp)) exp)) observed expected))))))

;TODO redo giving matrices are now neanderthal matrices
(defrecord Independance [observed]
  Categorical
  (pearson-chi-square [type]
    (let [exp (atom (conj (for [row-total (map #(reduce + %) observed)
                                column-total (map #(reduce + %) (matrix/columns observed))]
                            (/ (* row-total column-total)
                               (double (matrix/esum observed)))) :sentinal))]
      (assoc type :chi (matrix/esum (matrix/emap (fn [nums] (do (swap! exp next)
                                                                (/ (* (- nums (first @exp))
                                                                      (- nums (first @exp)))
                                                                   (first @exp)))) observed))))))

(defn expected-values [observed]
  (for [row-total (map #(reduce + %) observed)
        column-total (map #(reduce + %) (matrix/columns observed))]
    (/ (* row-total column-total)
       (double (matrix/esum observed)))))

(defn -expected-values [matrix nrows ncols]
  (let [mtrx (neanderthal-native/dge nrows ncols matrix)]
    (for [row-total (map #(neanderthal/sum (neanderthal-native/dv %)) (neanderthal/rows mtrx))
          column-total (neanderthal/mv! (neanderthal/trans mtrx)
                                        (neanderthal/entry! (neanderthal-native/dv nrows) 1.0)
                                        (neanderthal-native/dv ncols))]
      (/ (* row-total column-total)
         (neanderthal/sum
            (neanderthal-native/dv matrix))))))

(def observed-vals [60 300 10 390])
(def expected-vals '(33.1578947368421 36.8421052631579 326.8421052631579 363.1578947368421))

(def expected-vals-mtrx (neanderthal-native/dge 2 2 expected-vals))

(reduce +
        (map
          (fn [obs exp]
            (/ (* (- obs exp)
                  (- obs exp)) exp)) [60 10 300 390] expected-vals))

(defn -chi ^double [^double x ^double y]
  (/ (* (- x y)
        (- x y)) y))

(defn sum-cols [matrix nrows ncols]
  (neanderthal/mv! (neanderthal/trans (neanderthal-native/dge nrows ncols matrix))
                   (neanderthal/entry! (neanderthal-native/dv nrows) 1.0) (neanderthal-native/dv ncols)))

(neanderthal-native/dge 2 2 [60 300 10 390])

(defn pplus ^double [^double x ^double y]
  (+ x y))

(defn ptimes ^double [^double x ^double y]
  (* x y))

(defn pdiv ^double [^double x ^double y]
  (/ x y))

(defn pminus ^double [^double x ^double y]
  (- x y))

(defn sqr ^double [^double x]
  (* x x))

(defn add-one ^double [^double x]
  (+ x 1))

(neanderthal/subvector (neanderthal-native/dv [1 2 3 4 5 6 7 8 9]) 2 5)
(neanderthal/col (neanderthal/trans (neanderthal-native/dge 2 2 observed-vals)) 0)
(neanderthal/col (neanderthal/trans (neanderthal-native/dge 2 2 observed-vals)) 1)

(reduce +
  (fmap -chi [60 10 300 390] expected-vals))
(fmap -chi [60 10 300 390] expected-vals)

(fmap -chi
      (neanderthal/col (neanderthal/trans (neanderthal-native/dge 2 2 observed-vals)) 0)
      (neanderthal/col  expected-vals-mtrx 0))

(fmap -chi (neanderthal/trans (neanderthal-native/dge 2 2 observed-vals)) expected-vals-mtrx)

