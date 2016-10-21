(ns clojure.core.stats.lom.categorical.test
  (:require [clojure.core.stats.lom.categorical.test :refer [pearson-chi-square]])
  (:import [clojure.core.stats.lom.categorical.test  Independance]))

#_(defn chi-square-test

  "Computes a Pearson chi square test that is applied to a set of categorical
   (has been counted and divided into categories) data to evaluate how likely
   it is that any observed difference between the sets arose by chance.

   Takes a map of the form:
   {:observed <sequential collection> :nrows <long> :ncols <long>}

   Returns a map of the form:
   {:observed <sequential collection> :nrows <long> :ncols <long> :chi <double>}

   The <sequential collection> will be interpreted as a matrix of rows and columns:

   Sequential collection: [60.0 300.0 10.0 390.0]
   Rows: 60.0 10.0 | 300.0 390.0
   Columns: 60.0 300.0 | 10.0 390.0

   (chi-square-test {:observed [60.0 300.0 10.0 390.0] :nrows 2 :ncols 2}) => {:observed [60.0 300.0 10.0 390.0], :nrows 2, :ncols 2, :chi 45.47412008281575}"

  [{:keys [observed nrows ncols]}]
  (pearson-chi-square (Independance. observed nrows ncols)))

