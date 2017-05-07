(ns hypervibe.api.utils
    (:require [clojure.core.reducers :as r]
              [clojure.core.matrix :as matrix]))
(use 'clojure.core.matrix.operators)
(matrix/set-current-implementation :vectorz)

(defn ^double mean
    {:doc      "Mean"
     :arglists '([^mikera.vectorz.Vector data])}
    [data]
    (div= (reduce matrix/add
                     data))
    (matrix/ecount data))

(defn mean-1 [a])

(defn ^double ssdev [data mean]
    {:doc      "Sample standard deviation"
     :arglists '([data mean])}
    (Math/sqrt (mean-1 (map #(* (- mean %)
                                (- mean %))
                            data))))
