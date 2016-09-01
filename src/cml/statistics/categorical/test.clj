(ns cml.statistics.categorical.test
  (:require [clojure.core.matrix :as matrix]))


(defprotocol Categorical
  (pearson-chi-square [s] "Conducts a Chi Square test on a categorical data set"))


(defn expected-values [observed]
  (for [row-total (map #(reduce + %) observed)
        column-total (map #(reduce + %) (matrix/columns observed))]
    (/ (* row-total column-total)
       (double (matrix/esum observed)))))

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

(def observed-vals [60 300 10 390])
(def expected-vals '(33.1578947368421 36.8421052631579 326.8421052631579 363.1578947368421))

(def expected-val '(33.1578947368421 326.8421052631579 36.8421052631579 363.1578947368421))
