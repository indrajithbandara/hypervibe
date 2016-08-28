(ns cml.statistics.categorical.test
  (:require [clojure.core.matrix :as matrix]
            [uncomplicate.neanderthal.core :as neanderthal]
            [uncomplicate.neanderthal.native :as neanderthal-native]))


(defprotocol Categorical
  (pearson-chi-square [s] "Conducts a Chi Square test on a categorical data set"))

(defrecord Independance [observed expected]
  Categorical
  (pearson-chi-square [type]
    (let [exp (atom (conj expected :sentinal))]
      (assoc type
        :chi (matrix/esum (matrix/emap
                            (fn [nums] (do (swap! exp next)
                                           (/ (* (- nums (first @exp))
                                                 (- nums (first @exp)))
                                              (first @exp)))) observed))))))
