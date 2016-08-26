(ns cml.statistics.categorical.test
  (:require [clojure.core.matrix :as matrix]))


(defprotocol Categorical
  (pearson-chi-square [s] "Conducts a Chi Square test on a categorical data set"))

(defrecord Independance [observed expected]
  Categorical
  (pearson-chi-square [type]
    (let [exp (atom (conj expected :sentinal))]
      (assoc type
        :expected expected
        :observed observed
        :chi (matrix/esum (matrix/emap
                            (fn [nums]
                              (do (swap! exp next)
                                  (/ (* (- nums (first @exp))
                                        (- nums (first @exp)))
                                     (first @exp)))) observed))))))


