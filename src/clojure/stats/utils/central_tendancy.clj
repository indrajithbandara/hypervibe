(ns clojure.stats.utils.central_tendancy
  (:require [clojure.core.reducers :as r]))


(defn ^double mean [data] (/ (r/fold + data) (count data)))

(defn ^double mean-1 [data] (/ (r/fold + data) (dec (count data))))

(defn ^doubles difference [[sample-one sample-two]] (map - sample-one sample-two))


(defn permutations
  [x xs]
  (letfn [(factorial [x]
            (loop [cnt (if (coll? x)
                         (count x) x) acc 1]
              (if (zero? cnt)
                acc (recur (dec cnt) (*' cnt acc)))))]
    (quot (factorial x)
          (factorial (- x xs)))))

