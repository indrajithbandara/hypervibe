(ns cml.statistics.test
  (:require [cml.utils.tables :refer [t-table]]
            [clojure.core.matrix :as matrix]))

;TODO Have functions comply with dataframes
(defprotocol Ordinal)

(defprotocol Numerical
  (t-test [tt] "Conducts a TTest on a numerical data set"))

(defrecord OneSample [sample-mean sample-standard-deviation sample-hypothetical-mean sample-size]
  Numerical
  (t-test [type]
    (assoc type
      :t-statistic (/ (- sample-mean
                         sample-hypothetical-mean)
                      (/ sample-standard-deviation
                         (Math/sqrt sample-size)))
      :dof (dec sample-size))))


(defrecord EqualVariance [mean population-mean pooled-variance size]
  Numerical
  (t-test [type]
    (let [[mean-one mean-two] mean
          [population-mean-one population-mean-two] population-mean
          [pooled-variance-one pooled-variance-two] pooled-variance
          [size-one size-two] size]
      (assoc type
        :t-statistic (/ (- (- mean-one mean-two)
                           (- population-mean-one population-mean-two))
                        (Math/sqrt (* (/ (+ pooled-variance-one pooled-variance-two) 2)
                                      (+ (/ 1 size-one) (/ 1 size-two)))))
        :dof (- (+ size-one size-two) 2)))))


(defrecord Welch [mean sample-variance size]
  Numerical
  (t-test [type]
    (let [[mean-one mean-two] mean
          [sample-variance-one sample-variance-two] sample-variance
          [size-one size-two] size]
      (assoc type
        :t-statistic (/ (- mean-one mean-two)
                        (Math/sqrt (+ (/ sample-variance-one size-one)
                                      (/ sample-variance-two size-two))))
        :dof (/ (* (+ (/ sample-variance-one size-one)
                      (/ sample-variance-two size-two))
                   (+ (/ sample-variance-one size-one)
                      (/ sample-variance-two size-two)))
                (+ (/ (* (/ sample-variance-one size-one)
                         (/ sample-variance-one size-one))
                      (- size-one 1))
                   (/ (* (/ sample-variance-two size-two)
                         (/ sample-variance-two size-two))
                      (- size-two 1))))))))


(defrecord RepeatedMeasure [difference-mean population-mean standard-deviation size]
  Numerical
  (t-test [type]
    (let [[population-mean-one population-mean-two] population-mean]
      (assoc type
        :t-statistic (/ (- difference-mean
                           (- population-mean-one population-mean-two))
                        (/ standard-deviation
                           (Math/sqrt size)))
        :dof (- size 1)))))


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


